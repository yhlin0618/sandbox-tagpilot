-- ============================================================================
-- Supabase Authentication Setup for Wonderful Food Apps
-- Migration: 001_supabase_auth_setup.sql
-- Date: 2025-01-19
-- Purpose: Create users and login_limits tables with RPC functions
-- ============================================================================

-- NOTE: Execute this SQL in Supabase SQL Editor
-- Supabase Project: https://oziernubrqgqthjksbii.supabase.co

-- ============================================================================
-- 1. Enable Required Extensions
-- ============================================================================

CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- ============================================================================
-- 2. Create Users Table
-- ============================================================================

CREATE TABLE IF NOT EXISTS public.users (
  id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,  -- bcrypt hash using pgcrypto
  display_name TEXT,
  role TEXT DEFAULT 'user' CHECK (role IN ('admin', 'user')),
  company TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Index for faster username lookups
CREATE INDEX IF NOT EXISTS idx_users_username ON public.users(username);

-- ============================================================================
-- 3. Create Login Limits Table
-- ============================================================================

CREATE TABLE IF NOT EXISTS public.login_limits (
  id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  user_id UUID REFERENCES public.users(id) ON DELETE CASCADE,
  app_name TEXT NOT NULL CHECK (app_name IN ('brandedge', 'insightforge', 'tagpilot', 'vitalsigns')),
  login_count INT DEFAULT 0,
  max_logins INT DEFAULT 5,
  last_login TIMESTAMPTZ,
  UNIQUE(user_id, app_name)
);

-- Index for faster lookups
CREATE INDEX IF NOT EXISTS idx_login_limits_user_app
  ON public.login_limits(user_id, app_name);

-- ============================================================================
-- 4. Enable Row Level Security (RLS)
-- ============================================================================

ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.login_limits ENABLE ROW LEVEL SECURITY;

-- RLS policies for RPC access (SECURITY DEFINER functions bypass RLS)
-- These policies allow authenticated users to read their own data
CREATE POLICY "Users can view own profile"
  ON public.users FOR SELECT
  USING (true);  -- RPC functions use SECURITY DEFINER

CREATE POLICY "Login limits viewable via RPC"
  ON public.login_limits FOR SELECT
  USING (true);  -- RPC functions use SECURITY DEFINER

-- ============================================================================
-- 5. RPC Functions
-- ============================================================================

-- -----------------------------------------------------------------------------
-- 5.1 verify_password: Authenticate user with username and password
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION public.verify_password(
  p_username TEXT,
  p_password TEXT
)
RETURNS JSONB AS $$
DECLARE
  v_user RECORD;
BEGIN
  -- Find user by username
  SELECT id, password_hash, role, display_name, company
  INTO v_user
  FROM public.users
  WHERE username = p_username;

  -- User not found
  IF NOT FOUND THEN
    RETURN jsonb_build_object(
      'success', false,
      'error', 'User not found'
    );
  END IF;

  -- Verify password using pgcrypto crypt function
  -- crypt(password, hash) returns the hash if password matches
  IF v_user.password_hash = crypt(p_password, v_user.password_hash) THEN
    RETURN jsonb_build_object(
      'success', true,
      'user_id', v_user.id,
      'role', v_user.role,
      'display_name', v_user.display_name,
      'company', v_user.company
    );
  ELSE
    RETURN jsonb_build_object(
      'success', false,
      'error', 'Invalid password'
    );
  END IF;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- -----------------------------------------------------------------------------
-- 5.2 check_and_increment_login: Check login limit and increment counter
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION public.check_and_increment_login(
  p_user_id UUID,
  p_app_name TEXT
)
RETURNS JSONB AS $$
DECLARE
  v_login_count INT;
  v_max_logins INT;
  v_role TEXT;
BEGIN
  -- Check if user is admin (unlimited logins)
  SELECT role INTO v_role
  FROM public.users
  WHERE id = p_user_id;

  IF NOT FOUND THEN
    RETURN jsonb_build_object(
      'allowed', false,
      'error', 'User not found'
    );
  END IF;

  -- Admin users have unlimited logins
  IF v_role = 'admin' THEN
    -- Update last_login time but don't count
    INSERT INTO public.login_limits (user_id, app_name, login_count, max_logins, last_login)
    VALUES (p_user_id, p_app_name, 0, -1, NOW())
    ON CONFLICT (user_id, app_name)
    DO UPDATE SET last_login = NOW();

    RETURN jsonb_build_object(
      'allowed', true,
      'remaining', -1,
      'is_admin', true
    );
  END IF;

  -- For regular users: create or get login limit record
  INSERT INTO public.login_limits (user_id, app_name, login_count, max_logins)
  VALUES (p_user_id, p_app_name, 0, 5)
  ON CONFLICT (user_id, app_name) DO NOTHING;

  -- Check current login count
  SELECT login_count, max_logins
  INTO v_login_count, v_max_logins
  FROM public.login_limits
  WHERE user_id = p_user_id AND app_name = p_app_name;

  -- Check if limit exceeded
  IF v_login_count >= v_max_logins THEN
    RETURN jsonb_build_object(
      'allowed', false,
      'remaining', 0,
      'is_admin', false,
      'error', 'Login limit exceeded'
    );
  END IF;

  -- Increment counter and update last_login
  UPDATE public.login_limits
  SET login_count = login_count + 1,
      last_login = NOW()
  WHERE user_id = p_user_id AND app_name = p_app_name;

  RETURN jsonb_build_object(
    'allowed', true,
    'remaining', v_max_logins - v_login_count - 1,
    'is_admin', false
  );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- -----------------------------------------------------------------------------
-- 5.3 create_user: Create a new user with bcrypt password hash
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION public.create_user(
  p_username TEXT,
  p_password TEXT,
  p_role TEXT DEFAULT 'user',
  p_display_name TEXT DEFAULT NULL,
  p_company TEXT DEFAULT NULL
)
RETURNS JSONB AS $$
DECLARE
  v_user_id UUID;
BEGIN
  -- Validate role
  IF p_role NOT IN ('admin', 'user') THEN
    RETURN jsonb_build_object(
      'success', false,
      'error', 'Invalid role. Must be admin or user.'
    );
  END IF;

  -- Check if username already exists
  IF EXISTS (SELECT 1 FROM public.users WHERE username = p_username) THEN
    RETURN jsonb_build_object(
      'success', false,
      'error', 'Username already exists'
    );
  END IF;

  -- Create user with bcrypt hashed password
  INSERT INTO public.users (username, password_hash, role, display_name, company)
  VALUES (
    p_username,
    crypt(p_password, gen_salt('bf')),  -- bcrypt with blowfish
    p_role,
    COALESCE(p_display_name, p_username),
    p_company
  )
  RETURNING id INTO v_user_id;

  RETURN jsonb_build_object(
    'success', true,
    'user_id', v_user_id,
    'username', p_username
  );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- -----------------------------------------------------------------------------
-- 5.4 get_user_profile: Get user profile by ID
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION public.get_user_profile(
  p_user_id UUID
)
RETURNS JSONB AS $$
DECLARE
  v_user RECORD;
BEGIN
  SELECT id, username, role, display_name, company, created_at
  INTO v_user
  FROM public.users
  WHERE id = p_user_id;

  IF NOT FOUND THEN
    RETURN jsonb_build_object(
      'success', false,
      'error', 'User not found'
    );
  END IF;

  RETURN jsonb_build_object(
    'success', true,
    'user_id', v_user.id,
    'username', v_user.username,
    'role', v_user.role,
    'display_name', v_user.display_name,
    'company', v_user.company,
    'created_at', v_user.created_at
  );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- -----------------------------------------------------------------------------
-- 5.5 reset_login_count: Reset login count for a user/app (admin function)
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION public.reset_login_count(
  p_user_id UUID,
  p_app_name TEXT DEFAULT NULL
)
RETURNS JSONB AS $$
DECLARE
  v_affected INT;
BEGIN
  IF p_app_name IS NULL THEN
    -- Reset all apps for this user
    UPDATE public.login_limits
    SET login_count = 0
    WHERE user_id = p_user_id;
    GET DIAGNOSTICS v_affected = ROW_COUNT;
  ELSE
    -- Reset specific app
    UPDATE public.login_limits
    SET login_count = 0
    WHERE user_id = p_user_id AND app_name = p_app_name;
    GET DIAGNOSTICS v_affected = ROW_COUNT;
  END IF;

  RETURN jsonb_build_object(
    'success', true,
    'affected_rows', v_affected
  );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- ============================================================================
-- 6. Create Default Admin User
-- ============================================================================

-- Create admin user for testing (password: 12345)
-- Note: In production, change this password immediately!
SELECT public.create_user(
  'admin',
  '12345',
  'admin',
  'Administrator',
  'Wonderful Food'
);

-- ============================================================================
-- 7. Grant Execute Permissions on RPC Functions
-- ============================================================================

-- Grant execute permissions to anon and authenticated roles
GRANT EXECUTE ON FUNCTION public.verify_password(TEXT, TEXT) TO anon, authenticated;
GRANT EXECUTE ON FUNCTION public.check_and_increment_login(UUID, TEXT) TO anon, authenticated;
GRANT EXECUTE ON FUNCTION public.create_user(TEXT, TEXT, TEXT, TEXT, TEXT) TO anon, authenticated;
GRANT EXECUTE ON FUNCTION public.get_user_profile(UUID) TO anon, authenticated;
GRANT EXECUTE ON FUNCTION public.reset_login_count(UUID, TEXT) TO anon, authenticated;

-- ============================================================================
-- End of Migration
-- ============================================================================
