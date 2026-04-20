# Posit Connect Cloud 部署指南（GitHub 方式）

根據官方文檔，Posit Connect Cloud 使用 GitHub 整合進行部署。首次設定完成後，部署會自動由 Git 觸發，不需要手動 republish。

## 📋 部署前準備

### 1. 確認檔案結構
```
positioning_app/
├── app.R              # 主應用程式（必須）
├── manifest.json      # 依賴清單（必須）
├── data/              # 資料檔案
├── www/               # 靜態資源
├── icons/             # 圖標
└── scripts/           # 相關腳本
```

### 2. 創建 manifest.json
```r
# 在 positioning_app 目錄下執行
library(rsconnect)
rsconnect::writeManifest()
```

這會創建 `manifest.json` 檔案，記錄：
- R 版本
- 所需套件及版本

## 🚀 部署步驟

### 步驟 1：準備 GitHub Repository

1. 在 GitHub 創建新的 **公開** repository
2. 將 positioning_app 推送到 GitHub：

```bash
# 初始化 Git（如果還沒有）
git init

# 添加所有檔案
git add .

# 提交
git commit -m "Initial commit for positioning_app"

# 添加遠端 repository
git remote add origin https://github.com/YOUR_USERNAME/positioning_app.git

# 推送到 GitHub
git push -u origin main
```

### 步驟 2：在 Posit Connect Cloud 部署（首次設定）

1. 登入 [Posit Connect Cloud](https://connect.posit.cloud)
2. 點擊頁面頂部的 **Publish** 按鈕
3. 選擇 **Shiny**
4. 選擇您剛創建的 GitHub repository
5. 確認分支（通常是 `main` 或 `master`）
6. 選擇 **app.R** 作為主要檔案
7. 點擊 **Publish**
8. 確認已啟用 Git 變更自動部署（名稱依 UI 顯示為準）

### 步驟 3：監控部署

- 部署過程中會顯示狀態更新
- 底部會顯示建構日誌
- 部署完成後會獲得應用程式連結

## 📝 重要注意事項

### manifest.json 必須包含在 Git 中
修改 `.gitignore`，確保 `manifest.json` **不被**排除：
```bash
# 在 .gitignore 中，移除或註解這行
# manifest.json
```

### 確保 app.R 是主檔案
如果您使用 `full_app_v17.R`，需要：
```bash
# 複製為 app.R
cp full_app_v17.R app.R
```

## 🔄 更新應用程式

當您更新程式碼後：

1. 提交並推送到 GitHub：
```bash
git add .
git commit -m "Update app"
git push
```

2. Posit Connect Cloud 會自動偵測 Git 更新並重新部署（不需要手動點 republish）
3. 在 Content List 檢視部署狀態與建置日誌

## 🐛 疑難排解

### 問題：Repository 必須是公開的
Posit Connect Cloud 只能存取公開的 GitHub repositories。如果您的 repository 是私有的，需要改為公開。

### 問題：找不到 app.R
確保主檔案命名為 `app.R`，而不是其他名稱。

### 問題：套件版本衝突
重新生成 manifest.json：
```r
# 刪除舊的
file.remove("manifest.json")

# 創建新的
rsconnect::writeManifest()
```

## 📦 範例 .gitignore

```gitignore
# R 相關
.Rproj.user
.Rhistory
.RData
.Ruserdata

# 環境變數（保持私密）
.env
.env.*

# 資料檔案（視需求）
*.csv
*.xlsx
*.sqlite

# 但不要排除 manifest.json！
# manifest.json  <- 確保這行被註解或移除

# 暫存
cache/
temp/
```

## 🎯 完整檢查清單

部署前確認：
- [ ] `app.R` 存在且是最新版本
- [ ] `manifest.json` 已創建且是最新的
- [ ] 所有必要的資料檔案都已包含
- [ ] `.gitignore` 沒有排除 `manifest.json`
- [ ] GitHub repository 是公開的
- [ ] 所有變更都已提交並推送

---
最後更新：2024-01-15 
