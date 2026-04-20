# YAML 配置檔案結構說明

## 📂 目錄結構

```
config/yaml/
├── insightforge_config/
│   └── insightforge.yaml       # 主配置檔案
├── module_config/
│   ├── login.yaml             # 登入模組配置
│   ├── upload.yaml            # 資料上傳模組配置
│   ├── scoring.yaml           # 屬性評分模組配置
│   ├── sales_model.yaml       # 銷售模型模組配置
│   ├── keyword_ads.yaml       # 關鍵字廣告模組配置
│   └── product_dev.yaml       # 新品開發模組配置
└── README.md                  # 本文件
```

## 🎯 設計理念

### 1. 模組化配置
- **主配置檔案** (`insightforge.yaml`): 管理應用程式層級設定
- **模組配置檔案**: 每個模組獨立配置，易於維護和調整

### 2. 可調整參數
所有可能需要調整的參數都已外部化到 YAML 檔案：
- UI 元件設定
- 資料限制（如最大品牌數、每品牌記錄數）
- AI 模型參數
- 效能優化設定
- 通知訊息模板

## 📋 主要配置項目

### insightforge.yaml
- **app_info**: 應用程式基本資訊
- **environment**: 環境設定（開發/生產）
- **database**: 資料庫連線設定
- **api**: API 配置（OpenAI 等）
- **ui**: 使用者介面主題設定
- **features**: 功能開關
- **modules**: 模組載入順序與相依性
- **navigation**: 導航流程定義

### 模組配置檔案

#### upload.yaml
- **data_limits**: 資料上傳限制
  - `max_brands`: 10 個品牌
  - `max_records_per_brand`: 評論 500 筆/銷售 2000 筆
- **file_settings**: 檔案大小與格式限制
- **validation**: 資料驗證規則
- **processing**: 資料處理策略

#### scoring.yaml
- **attribute_extraction**: 屬性萃取設定
  - `num_attributes`: 10-30 個屬性（預設 15）
  - `sampling_for_extraction`: 每品牌 30 筆評論
- **scoring**: 評分設定
  - `review_sampling`: 2-500 筆（預設 50）
  - `score_range`: 1-5 分
- **ai_settings**: AI 模型參數
- **performance**: 平行處理設定

#### sales_model.yaml
- **poisson_regression**: Poisson 回歸模型設定
- **data_matching**: 資料匹配策略
- **results_calculation**: 邊際效應與賽道倍數計算
- **visualization**: 視覺化設定
- **marketing_strategy**: 行銷策略生成

#### keyword_ads.yaml
- **platforms**: 廣告平台設定（Google、Facebook、Instagram）
- **keyword_generation**: 關鍵字生成策略
- **ad_copy**: 廣告文案生成
- **audience_targeting**: 目標受眾設定
- **budget_recommendation**: 預算建議

#### product_dev.yaml
- **analysis_methods**: 分析方法（缺口分析、SWOT、創新矩陣）
- **concept_generation**: 產品概念生成
- **attribute_optimization**: 屬性優化建議
- **roadmap**: 開發路線圖
- **risk_assessment**: 風險評估

## 🔧 使用方式

### 1. 讀取配置
```r
# 讀取主配置
config <- yaml::read_yaml("config/yaml/insightforge_config/insightforge.yaml")

# 讀取模組配置
upload_config <- yaml::read_yaml("config/yaml/module_config/upload.yaml")
```

### 2. 在模組中使用配置
```r
# 例如在 upload 模組中
max_brands <- upload_config$data_limits$reviews$max_brands
max_records <- upload_config$data_limits$reviews$max_records_per_brand

# 限制資料量
data <- data %>%
  group_by(Variation) %>%
  slice_head(n = max_records) %>%
  ungroup() %>%
  filter(Variation %in% unique(Variation)[1:max_brands])
```

### 3. 動態調整參數
無需修改程式碼，只需編輯 YAML 檔案即可調整：
- 評分屬性數量範圍
- 資料上傳限制
- AI 模型參數
- UI 顯示設定
- 通知訊息內容

## 🚀 優勢

1. **集中管理**: 所有配置集中在 config/yaml 目錄
2. **易於維護**: 修改配置不需要改程式碼
3. **版本控制**: YAML 檔案易於追蹤變更
4. **環境切換**: 可為不同環境準備不同配置
5. **參數透明**: 所有可調參數一目了然

## 📝 注意事項

1. 修改 YAML 檔案時請保持縮排正確（使用空格，不要用 Tab）
2. 布林值使用 `true/false` 而非 `TRUE/FALSE`
3. 字串包含特殊字元時使用引號
4. 修改配置後需要重啟應用程式才會生效

## 🔄 更新歷史

- **2025-09-28**: 初始版本建立
  - 分離主配置與模組配置
  - 外部化所有可調參數
  - 新增詳細的參數說明