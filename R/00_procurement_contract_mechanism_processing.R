# -*- coding: UTF-8 -*-
################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    00_procurement_contract_mechanism_processing.R
# Purpose: Classify sponge-related procurement contracts and build city-year
#          procurement mechanism variables for Fig. 4
# Author:  Yanghan Lin et al.
################################################################################

library(haven)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)


#==============================================================================
# 0. Paths and user options
#==============================================================================

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
procurement_dir <- file.path(project_root, "data", "raw",
                             "government_procurement_contracts")
if (!dir.exists(procurement_dir)) {
  procurement_dir <- file.path(project_root, "政府采购合同数据")
}
output_dir <- file.path(project_root, "submission_code", "outputs",
                        "procurement_contract_processing")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Change this vector after adding 2020-2024 files.
years_to_process <- 2015:2024

# "core" keeps only clearly relevant urban-infrastructure agencies.
#     "wide" additionally keeps local governments, development-zone committees,
#     and city-owned utility/platform companies.
buyer_scope <- "core"

# Amount-correction thresholds follow the practice in procurement-data
#     papers: very large and very small values are flagged and, when possible,
#     given a rule-based corrected value for review.
large_amount_threshold_wan <- 200000
large_amount_auto_divide_wan <- 2000000
small_amount_threshold_wan <- 0.1
audit_n_per_group <- 80
set.seed(20260530)


#==============================================================================
# 1. Utility functions
#==============================================================================

escape_regex <- function(x) {
  str_replace_all(x, "([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1")
}

make_pat <- function(kws) {
  kws <- unique(kws[!is.na(kws) & nchar(kws) > 0])
  if (length(kws) == 0) return(regex("a^", ignore_case = TRUE))
  regex(str_c(escape_regex(kws), collapse = "|"), ignore_case = TRUE)
}

detect_any <- function(x, kws) {
  str_detect(replace_na(as.character(x), ""), make_pat(kws))
}

pick_first <- function(candidates, names_vec) {
  hit <- intersect(candidates, names_vec)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

write_csv_utf8 <- function(x, path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_excel_csv(x, path)
  } else {
    write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  }
}

parse_first_number <- function(x) {
  x <- replace_na(as.character(x), "")
  num <- str_extract(x, "[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?")
  as.numeric(str_replace_all(num, ",", ""))
}

amount_text_to_wan <- function(x) {
  x <- replace_na(as.character(x), "")
  num <- parse_first_number(x)
  case_when(
    is.na(num) ~ NA_real_,
    str_detect(x, "亿元|亿") ~ num * 10000,
    str_detect(x, "万元|万") ~ num,
    str_detect(x, "元") ~ num / 10000,
    TRUE ~ NA_real_
  )
}

is_phone_like_amount <- function(x) {
  x_chr <- str_replace_all(format(x, scientific = FALSE, trim = TRUE), "\\D", "")
  str_detect(x_chr, "^1[0-9]{10}$")
}

make_row_id <- function(df, id_col) {
  if (!is.na(id_col)) return(as.character(df[[id_col]]))
  as.character(seq_len(nrow(df)))
}


#==============================================================================
# 2. Buyer filters
#==============================================================================

buyer_core_kw <- c(
  # Housing, urban construction, drainage, water affairs
  "住房和城乡建设局", "住房城乡建设局", "住建局", "建设局",
  "城乡建设局", "城市建设局", "城建局", "建委", "建设委员会",
  "住房和城乡建设委员会", "城乡建设委员会",
  "水务局", "水利局", "水利水务局", "水务管理局",
  "排水管理处", "排水管理办公室", "排水管理中心", "排水公司",
  "排管办",
  "供排水", "给排水", "防汛", "防洪", "排涝",

  # Municipal, urban management, public utilities
  "市政", "市政设施", "市政工程", "市政管理", "市政园林",
  "城市管理局", "城管局", "城市管理委员会", "城市管理综合执法局",
  "公用事业", "公用事业局", "城市公用事业",

  # Planning, garden, sanitation, natural-resource planning
  "规划局", "自然资源和规划局", "国土资源和规划局",
  "城乡规划", "规划建设", "园林局", "园林绿化局",
  "园林管理", "园林环卫", "环境卫生", "环卫局", "环卫管理",

  # Local development zones and local construction management bodies
  "管委会", "管理委员会", "开发区", "新区", "高新区",
  "项目建设管理中心", "重点工程建设管理", "城市建设管理"
)

buyer_wide_extra_kw <- c(
  # Used for robustness or manual checks.
  "人民政府", "街道办事处", "镇人民政府", "乡人民政府",
  "城投", "城市投资", "城建投资", "水务集团", "水投",
  "市政集团", "建设投资", "基础设施投资", "交通投资",
  "环境投资", "生态环境局", "环保局"
)

buyer_exclude_kw <- c(
  # These buyers are often only geographically located in the city rather
  #     than representing local Sponge City implementation.
  "大学", "学院", "学校", "中学", "小学", "幼儿园",
  "医院", "卫生院", "疾控中心", "海关", "税务", "气象局",
  "地震局", "博物院", "博物馆", "电视台", "报社",
  "中国科学院", "中国工程院", "中央", "部委",
  "水利部", "长江水利委员会", "黄河水利委员会", "淮河水利委员会",
  "海河水利委员会", "珠江水利委员会", "松辽水利委员会",
  "太湖流域管理局", "民航", "铁路", "军队", "武警",
  "法院", "检察院", "公安", "消防", "监狱"
)


#==============================================================================
# 3. Mechanism keywords: six mutually exclusive primary categories
#==============================================================================

# Category 1: Sponge/LID source-control facilities
sponge_broad_kw <- c(
  "海绵城市", "海绵试点", "海绵化", "海绵体", "低影响开发",
  "LID"
)

source_lid_kw <- c(
  "透水铺装", "透水路面", "透水砖", "透水混凝土", "透水沥青",
  "雨水花园", "下凹式绿地", "生物滞留", "生物滞留带",
  "生态滞留", "植草沟", "植被缓冲带", "绿色屋顶", "屋顶绿化",
  "雨水收集", "雨水利用", "雨水回用", "雨水桶", "初雨弃流",
  "渗透塘", "渗井", "渗渠", "渗沟", "雨水调蓄池", "调蓄池",
  "调蓄设施", "蓄水模块", "模块蓄水", "雨洪利用"
)

# Category 2: Gray drainage expansion and upgrading
gray_domain_kw <- c(
  "排水", "雨水管", "排水管", "污水管", "雨污", "雨污分流",
  "排水管网", "雨水管网", "污水管网", "管网", "泵站", "雨水泵站",
  "排涝泵站", "水泵", "泵车", "移动泵", "排涝车",
  "一体化泵站", "闸站", "闸门", "排涝", "防涝", "内涝",
  "积水点", "易涝点", "下穿桥", "下立交", "雨水口", "雨水井",
  "雨水篦", "雨水箅", "检查井", "排水沟",
  "排水渠", "箱涵", "暗涵", "涵洞", "截流", "截污", "溢流",
  "合流制", "排口", "出水口"
)

construction_action_kw <- c(
  "新建", "扩建", "改建", "改造", "改扩建", "建设", "施工",
  "工程", "完善", "配套", "提升", "提标", "整治", "治理",
  "修复", "迁改", "更新", "改良", "安装", "采购", "购置",
  "PPP", "EPC"
)

# Category 3: Key-node operation and maintenance
operation_kw <- c(
  "清淤", "清掏", "疏浚", "疏通", "养护", "维护", "维修",
  "抢修", "保养", "CCTV检测", "CCTV 检测", "闭路电视检测",
  "管网检测", "管道检测", "管网普查", "管网排查", "排水排查",
  "错接", "混接", "错混接", "病害检测", "病害修复", "非开挖修复",
  "易涝点整治", "积水点整治", "应急排涝", "应急抢险",
  "移动泵车", "泵车租赁", "租赁", "运营", "运营管理", "运行管理",
  "运行维护", "运维管理", "泵站运行", "闸门维修", "闸站维护",
  "排水设施养护", "市政设施养护"
)

# Category 4: Smart monitoring, warning, and dispatching
monitoring_kw <- c(
  "监测", "在线监测", "自动监测", "水位监测", "液位监测",
  "流量监测", "雨量监测", "积水监测", "内涝监测", "水质监测",
  "传感器", "液位计", "水位计", "流量计", "雨量计",
  "物联网", "智慧水务", "智慧排水", "排水信息化", "水务信息化",
  "信息化平台", "监测平台", "预警平台", "预警系统", "调度平台",
  "调度系统", "防汛指挥", "防汛调度", "排水调度", "城市暴雨内涝",
  "下立交积水自动监测", "下穿桥积水监测"
)

# Category 5: Blue-green retention and water-system restoration
blue_green_kw <- c(
  "河道", "河湖", "湖泊", "水库", "水质保护", "水系", "水网",
  "湿地", "生态湿地",
  "公园绿地", "绿地调蓄", "海绵公园", "雨洪公园", "调蓄湖",
  "蓄滞洪", "滞洪", "滞洪区", "蓄洪", "行洪", "泄洪通道",
  "水环境", "水生态", "生态修复", "水体修复", "黑臭水体",
  "河道治理", "河湖治理", "河道整治", "水系连通",
  "生态岸线", "护岸生态", "岸线修复", "滨水空间"
)

# Category 6: Planning, design, and technical services
planning_service_kw <- c(
  "专项规划", "规划编制", "海绵城市规划", "排水防涝规划",
  "实施方案", "PPP实施方案", "PPP 实施方案", "可研", "可行性研究",
  "方案编制", "方案设计", "初步设计", "施工图设计", "勘察设计",
  "设计咨询", "技术咨询", "技术服务", "第三方评估", "第三方服务",
  "绩效评价", "本底调查", "本底监测", "效果评估", "课题研究",
  "专题研究", "监理", "全过程咨询", "项目管理咨询"
)

# Large procurement values are kept only when the text strongly indicates
#     a genuinely large construction, PPP, EPC, or concession contract.
plausible_large_kw <- c(
  "PPP", "EPC", "特许经营", "项目协议", "投资合作协议", "投资合作",
  "工程总承包"
)

plausible_gov_service_large_kw <- c(
  "市属公共污水处理系统运营管理政府购买服务项目", "公共污水处理系统运营管理",
  "政府购买服务项目", "政府购买服务合同"
)

ppp_planning_kw <- c("PPP实施方案", "PPP 实施方案")

# Ordinary material purchases or system-procurement contracts are not kept
#     as billion-yuan projects even if they contain broad construction words.
large_small_purchase_kw <- c(
  "办公用品", "物资采购", "货物类", "采购及安装", "监控系统",
  "显示系统", "广播系统", "大屏", "管理平台", "信息系统",
  "一库多平台"
)


#==============================================================================
# 4. Dictionary output
#==============================================================================

keyword_dictionary <- bind_rows(
  tibble(category = "buyer_core", keyword = buyer_core_kw),
  tibble(category = "buyer_wide_extra", keyword = buyer_wide_extra_kw),
  tibble(category = "buyer_exclude", keyword = buyer_exclude_kw),
  tibble(category = "sponge_broad", keyword = sponge_broad_kw),
  tibble(category = "source_lid_facilities", keyword = source_lid_kw),
  tibble(category = "gray_domain", keyword = gray_domain_kw),
  tibble(category = "construction_action", keyword = construction_action_kw),
  tibble(category = "key_node_operation_maintenance", keyword = operation_kw),
  tibble(category = "smart_monitoring_warning_dispatch", keyword = monitoring_kw),
  tibble(category = "blue_green_retention_restoration", keyword = blue_green_kw),
  tibble(category = "planning_design_technical_services",
         keyword = planning_service_kw),
  tibble(category = "plausible_large_amount", keyword = plausible_large_kw),
  tibble(category = "plausible_government_service_large_amount",
         keyword = plausible_gov_service_large_kw),
  tibble(category = "ppp_planning_exclusion", keyword = ppp_planning_kw),
  tibble(category = "large_amount_small_purchase_exclusion",
         keyword = large_small_purchase_kw)
)

write_csv_utf8(
  keyword_dictionary,
  file.path(output_dir, "procurement_mechanism_keyword_dictionary.csv")
)


#==============================================================================
# 5. Amount correction
#==============================================================================

standardize_amount <- function(df, amount_col, raw_amount_col,
                               quantity_col, unit_price_col,
                               text_for_amount = NULL) {
  amount_num_wan <- if (!is.na(amount_col)) {
    suppressWarnings(as.numeric(df[[amount_col]]))
  } else {
    rep(NA_real_, nrow(df))
  }

  raw_amount_text <- if (!is.na(raw_amount_col)) {
    as.character(df[[raw_amount_col]])
  } else {
    rep(NA_character_, nrow(df))
  }

  amount_from_text_wan <- amount_text_to_wan(raw_amount_text)

  quantity_num <- if (!is.na(quantity_col)) {
    parse_first_number(df[[quantity_col]])
  } else {
    rep(NA_real_, nrow(df))
  }

  unit_price_wan <- if (!is.na(unit_price_col)) {
    amount_text_to_wan(df[[unit_price_col]])
  } else {
    rep(NA_real_, nrow(df))
  }

  amount_from_qty_price_wan <- ifelse(
    !is.na(quantity_num) & !is.na(unit_price_wan),
    quantity_num * unit_price_wan,
    NA_real_
  )

  amount_base_wan <- coalesce(amount_from_text_wan, amount_num_wan,
                              amount_from_qty_price_wan)

  phone_like <- is_phone_like_amount(amount_base_wan)
  multi_lot <- str_detect(raw_amount_text, "第.{0,3}包|标段|包组|分包|多标段")
  text_for_amount <- if (is.null(text_for_amount)) raw_amount_text else
    replace_na(as.character(text_for_amount), "")
  project_financing <- detect_any(text_for_amount, plausible_large_kw) &
    !detect_any(text_for_amount, ppp_planning_kw)
  gov_service_large <- detect_any(text_for_amount,
                                  plausible_gov_service_large_kw)
  small_purchase_or_system <- detect_any(text_for_amount,
                                         large_small_purchase_kw)
  plausible_large <- (project_financing | gov_service_large) &
    !small_purchase_or_system

  correction_flag <- case_when(
    is.na(amount_base_wan) & !is.na(amount_from_qty_price_wan) ~
      "filled_from_quantity_unit_price",
    is.na(amount_base_wan) ~ "missing_amount_need_manual",
    phone_like ~ "possible_phone_or_id_need_manual",
    amount_base_wan >= large_amount_auto_divide_wan ~
      "huge_possible_unit_error_divide_10000",
    amount_base_wan >= large_amount_threshold_wan & plausible_large ~
      "large_kept_plausible",
    amount_base_wan >= large_amount_threshold_wan ~
      "large_possible_unit_error_divide_10000",
    amount_base_wan > 0 & amount_base_wan < small_amount_threshold_wan ~
      "tiny_possible_unit_error_multiply_10000",
    multi_lot ~ "multi_lot_check",
    TRUE ~ "ok"
  )

  amount_corrected_wan <- case_when(
    correction_flag == "possible_phone_or_id_need_manual" ~ NA_real_,
    correction_flag == "huge_possible_unit_error_divide_10000" ~
      amount_base_wan / 10000,
    correction_flag == "large_possible_unit_error_divide_10000" ~
      amount_base_wan / 10000,
    correction_flag == "tiny_possible_unit_error_multiply_10000" ~
      amount_base_wan * 10000,
    TRUE ~ amount_base_wan
  )

  tibble(
    amount_raw_num_wan = amount_num_wan,
    amount_from_text_wan = amount_from_text_wan,
    amount_from_qty_price_wan = amount_from_qty_price_wan,
    amount_base_wan = amount_base_wan,
    amount_corrected_wan = amount_corrected_wan,
    amount_correction_flag = correction_flag,
    amount_needs_manual_review = correction_flag != "ok"
  )
}


#==============================================================================
# 6. Read, classify, and audit one year
#==============================================================================

classify_one_year <- function(year_i) {
  message("Processing procurement contracts for ", year_i, " ...")
  dta_file <- file.path(procurement_dir, paste0(year_i, ".dta"))
  if (!file.exists(dta_file)) stop("File not found: ", dta_file)

  candidate_cols <- c(
    "年份", "合同名称", "详情链接", "签订时间", "发布时间",
    "采购人", "采购单位", "采购人名称", "供应商", "供应商名称",
    "合同编号", "合同公告编号", "公告编号", "项目编号",
    "采购人地址", "供应商地址", "主要标的名称", "规格型号或服务要求",
    "主要标的数量", "主要标的单价", "合同金额",
    "履约期限和地点等简要信息", "采购方式", "所属地域", "所属行业",
    "代理机构", "合同签订日期", "合同公告日期",
    "合同金额num_万元", "合同金额_万元", "金额_万元", "amount",
    "id", "ID", "采购人_省", "采购人_省代码", "采购人_市",
    "采购人_市代码", "采购人_县", "采购人_县代码"
  )

  df <- tryCatch(
    read_dta(dta_file, col_select = any_of(candidate_cols)),
    error = function(e) read_dta(dta_file)
  )

  text_cols <- intersect(
    c("合同名称", "采购人", "采购单位", "采购人名称", "供应商",
      "供应商名称", "主要标的名称", "规格型号或服务要求",
      "履约期限和地点等简要信息", "采购方式", "所属地域",
      "所属行业", "代理机构"),
    names(df)
  )

  buyer_col <- pick_first(c("采购人", "采购单位", "采购人名称"), names(df))
  amount_col <- pick_first(
    c("合同金额num_万元", "合同金额_万元", "金额_万元", "amount"),
    names(df)
  )
  raw_amount_col <- pick_first(c("合同金额", "金额"), names(df))
  quantity_col <- pick_first(c("主要标的数量", "数量"), names(df))
  unit_price_col <- pick_first(c("主要标的单价", "单价"), names(df))
  id_col <- pick_first(c("合同编号", "合同公告编号", "公告编号", "id", "ID"),
                       names(df))
  year_col <- pick_first(c("年份", "year"), names(df))
  city_code_col <- pick_first(c("采购人_市代码", "市代码", "city_code"),
                              names(df))
  city_col <- pick_first(c("采购人_市", "市", "city"), names(df))
  county_col <- pick_first(c("采购人_县", "县", "county"), names(df))

  if (is.na(buyer_col)) stop("Buyer column not found.")
  if (is.na(year_col)) stop("Year column not found.")
  if (is.na(city_code_col)) stop("City-code column not found.")
  if (length(text_cols) == 0) stop("No text columns found.")

  text_all_for_amount <- df %>%
    mutate(across(all_of(text_cols), ~ replace_na(as.character(.x), ""))) %>%
    unite("text_all_for_amount", all_of(text_cols), sep = " ",
          remove = FALSE) %>%
    pull(text_all_for_amount) %>%
    str_squish()

  amount_vars <- standardize_amount(
    df = df,
    amount_col = amount_col,
    raw_amount_col = raw_amount_col,
    quantity_col = quantity_col,
    unit_price_col = unit_price_col,
    text_for_amount = text_all_for_amount
  )

  row_ids <- make_row_id(df, id_col)

  df <- bind_cols(df, amount_vars) %>%
    mutate(
      row_id_procurement = row_ids,
      across(all_of(text_cols), ~ replace_na(as.character(.x), "")),
      buyer_text = replace_na(as.character(.data[[buyer_col]]), "")
    ) %>%
    unite("text_all", all_of(text_cols), sep = " ", remove = FALSE) %>%
    mutate(text_all = str_squish(text_all))

  # English LID is handled separately to avoid false positives such as LiDAR.
  hit_lid_english <- str_detect(
    df$text_all,
    regex("(?<![A-Za-z])LID(?![A-Za-z])", ignore_case = TRUE)
  )

  df_classified <- df %>%
    mutate(
      buyer_core = detect_any(buyer_text, buyer_core_kw),
      buyer_wide = buyer_core | detect_any(buyer_text, buyer_wide_extra_kw),
      buyer_excluded = detect_any(buyer_text, buyer_exclude_kw),
      buyer_in_scope = if_else(
        buyer_scope == "wide",
        buyer_wide & !buyer_excluded,
        buyer_core & !buyer_excluded
      ),

      hit_sponge_broad = detect_any(text_all, sponge_broad_kw) |
        hit_lid_english,
      hit_source_lid = detect_any(text_all, source_lid_kw),
      hit_gray_domain = detect_any(text_all, gray_domain_kw),
      hit_construction_action = detect_any(text_all, construction_action_kw),
      hit_operation = detect_any(text_all, operation_kw),
      hit_monitoring = detect_any(text_all, monitoring_kw),
      hit_blue_green = detect_any(text_all, blue_green_kw),
      hit_planning_service = detect_any(text_all, planning_service_kw),

      mechanism_smart_monitoring = buyer_in_scope &
        (hit_sponge_broad | hit_source_lid | hit_gray_domain |
           hit_blue_green) & hit_monitoring,

      mechanism_key_node_operation = buyer_in_scope &
        hit_gray_domain & hit_operation,

      mechanism_source_lid = buyer_in_scope &
        (hit_source_lid |
           (hit_sponge_broad & !hit_planning_service & !hit_monitoring &
              !hit_blue_green & !hit_gray_domain)),

      mechanism_blue_green = buyer_in_scope &
        hit_blue_green & !hit_source_lid,

      mechanism_gray_expansion = buyer_in_scope &
        hit_gray_domain & hit_construction_action & !hit_operation,

      mechanism_planning_design = buyer_in_scope &
        (hit_sponge_broad | hit_source_lid | hit_gray_domain |
           hit_blue_green) & hit_planning_service,

      related_any = mechanism_smart_monitoring |
        mechanism_key_node_operation |
        mechanism_source_lid |
        mechanism_blue_green |
        mechanism_gray_expansion |
        mechanism_planning_design,

      # Mutually exclusive primary mechanism. Priority is designed to keep
      #     non-expansion improvements from being swallowed by construction terms.
      mechanism_primary = case_when(
        !buyer_in_scope | !related_any ~ "not_related_or_outside_scope",
        mechanism_smart_monitoring ~ "smart_monitoring_warning_dispatch",
        mechanism_key_node_operation ~ "key_node_operation_maintenance",
        mechanism_source_lid ~ "source_lid_facilities",
        mechanism_blue_green ~ "blue_green_retention_restoration",
        mechanism_gray_expansion ~ "gray_drainage_expansion",
        mechanism_planning_design ~ "planning_design_technical_services",
        TRUE ~ "not_related_or_outside_scope"
      ),

      mechanism_tags = pmap_chr(
        list(
          mechanism_source_lid,
          mechanism_gray_expansion,
          mechanism_key_node_operation,
          mechanism_smart_monitoring,
          mechanism_blue_green,
          mechanism_planning_design
        ),
        function(a, b, c, d, e, f) {
          labs <- c(
            "source_lid_facilities",
            "gray_drainage_expansion",
            "key_node_operation_maintenance",
            "smart_monitoring_warning_dispatch",
            "blue_green_retention_restoration",
            "planning_design_technical_services"
          )
          hits <- c(a, b, c, d, e, f)
          if (!any(hits)) return("")
          str_c(labs[hits], collapse = "; ")
        }
      )
    )

  summary_long <- df_classified %>%
    filter(mechanism_primary != "not_related_or_outside_scope") %>%
    group_by(
      year = .data[[year_col]],
      city_code = .data[[city_code_col]],
      city = .data[[city_col]],
      mechanism_primary
    ) %>%
    summarise(
      contract_count = n_distinct(row_id_procurement, na.rm = TRUE),
      amount_raw_wan = sum(amount_base_wan, na.rm = TRUE),
      amount_corrected_wan = sum(amount_corrected_wan, na.rm = TRUE),
      manual_amount_review_count = sum(amount_needs_manual_review, na.rm = TRUE),
      .groups = "drop"
    )

  summary_wide <- summary_long %>%
    pivot_wider(
      names_from = mechanism_primary,
      values_from = c(contract_count, amount_raw_wan, amount_corrected_wan,
                      manual_amount_review_count),
      values_fill = 0
    )

  audit_cols <- c(
    "row_id_procurement", year_col, city_code_col, city_col, county_col,
    buyer_col, raw_amount_col, amount_col,
    "amount_base_wan", "amount_corrected_wan", "amount_correction_flag",
    "buyer_in_scope", "buyer_core", "buyer_wide", "buyer_excluded",
    "mechanism_primary", "mechanism_tags",
    "hit_sponge_broad", "hit_source_lid", "hit_gray_domain",
    "hit_construction_action", "hit_operation", "hit_monitoring",
    "hit_blue_green", "hit_planning_service", "text_all"
  )
  audit_cols <- audit_cols[!is.na(audit_cols)]
  audit_cols <- intersect(audit_cols, names(df_classified))

  audit_sample <- df_classified %>%
    filter(mechanism_primary != "not_related_or_outside_scope") %>%
    select(all_of(audit_cols)) %>%
    group_by(mechanism_primary, amount_correction_flag) %>%
    group_modify(~ slice_sample(.x, n = min(audit_n_per_group, nrow(.x)))) %>%
    ungroup()

  amount_audit <- df_classified %>%
    filter(amount_needs_manual_review,
           mechanism_primary != "not_related_or_outside_scope") %>%
    arrange(desc(amount_base_wan)) %>%
    select(all_of(audit_cols))

  write_csv_utf8(
    summary_long,
    file.path(output_dir, paste0("summary_long_", year_i, ".csv"))
  )
  write_csv_utf8(
    summary_wide,
    file.path(output_dir, paste0("summary_wide_", year_i, ".csv"))
  )
  write_csv_utf8(
    audit_sample,
    file.path(output_dir, paste0("audit_sample_", year_i, ".csv"))
  )
  write_csv_utf8(
    amount_audit,
    file.path(output_dir, paste0("amount_correction_audit_", year_i, ".csv"))
  )

  invisible(list(
    summary_long = summary_long,
    summary_wide = summary_wide,
    audit_sample = audit_sample,
    amount_audit = amount_audit
  ))
}


#==============================================================================
# 7. Run classification and export outputs
#==============================================================================

results <- map(years_to_process, classify_one_year)

summary_all <- bind_rows(map(results, "summary_long"))
write_csv_utf8(
  summary_all,
  file.path(output_dir, "summary_long_all_years.csv")
)

summary_all_wide <- summary_all %>%
  pivot_wider(
    names_from = mechanism_primary,
    values_from = c(contract_count, amount_raw_wan, amount_corrected_wan,
                    manual_amount_review_count),
    values_fill = 0
  )
write_csv_utf8(
  summary_all_wide,
  file.path(output_dir, "summary_wide_all_years.csv")
)

# Create a Stata-ready city-year file with variable names used in the annual
# mechanism regressions. Amounts are measured in 10,000 RMB.
col_or_zero <- function(data, col) {
  if (col %in% names(data)) {
    as.numeric(data[[col]])
  } else {
    rep(0, nrow(data))
  }
}

panel_ready <- summary_all_wide %>%
  mutate(
    proc_source_lid = col_or_zero(., "amount_corrected_wan_source_lid_facilities"),
    proc_gray_drain = col_or_zero(., "amount_corrected_wan_gray_drainage_expansion"),
    proc_keynode_om = col_or_zero(., "amount_corrected_wan_key_node_operation_maintenance"),
    proc_smart_monitor = col_or_zero(., "amount_corrected_wan_smart_monitoring_warning_dispatch"),
    proc_bluegreen = col_or_zero(., "amount_corrected_wan_blue_green_retention_restoration"),
    proc_plan_design = col_or_zero(., "amount_corrected_wan_planning_design_technical_services"),
    n_source_lid = col_or_zero(., "contract_count_source_lid_facilities"),
    n_gray_drain = col_or_zero(., "contract_count_gray_drainage_expansion"),
    n_keynode_om = col_or_zero(., "contract_count_key_node_operation_maintenance"),
    n_smart_monitor = col_or_zero(., "contract_count_smart_monitoring_warning_dispatch"),
    n_bluegreen = col_or_zero(., "contract_count_blue_green_retention_restoration"),
    n_plan_design = col_or_zero(., "contract_count_planning_design_technical_services"),
    ln_proc_source_lid = log(proc_source_lid + 1),
    ln_proc_gray_drain = log(proc_gray_drain + 1),
    ln_proc_keynode_om = log(proc_keynode_om + 1),
    ln_proc_smart_monitor = log(proc_smart_monitor + 1),
    ln_proc_bluegreen = log(proc_bluegreen + 1),
    ln_proc_plan_design = log(proc_plan_design + 1)
  ) %>%
  select(
    year, city_code, city,
    proc_source_lid, proc_gray_drain, proc_keynode_om,
    proc_smart_monitor, proc_bluegreen, proc_plan_design,
    n_source_lid, n_gray_drain, n_keynode_om,
    n_smart_monitor, n_bluegreen, n_plan_design,
    ln_proc_source_lid, ln_proc_gray_drain, ln_proc_keynode_om,
    ln_proc_smart_monitor, ln_proc_bluegreen, ln_proc_plan_design
  )

write_csv_utf8(
  panel_ready,
  file.path(output_dir, "procurement_mechanisms_city_year_for_panel.csv")
)

message("Done. Procurement classification outputs were written to: ", output_dir)
