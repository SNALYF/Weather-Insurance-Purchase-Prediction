# run_pipeline.R
# 主控脚本：运行所有分析步骤

# 1. 设置环境和安装依赖
message(">>> Step 1: Setting up environment...")
source("setup.R")

# 2. 运行主要分析脚本
# 使用 tryCatch 确保即使出错也能继续或报告
run_script <- function(script_name) {
  message(paste(">>> Running script:", script_name, "..."))
  tryCatch({
    source(script_name, local = TRUE) # local=TRUE to avoid polluting global env too much, though scripts might clear it
    message(paste(">>> Successfully finished:", script_name))
  }, error = function(e) {
    message(">>> Error running script:")
    message(conditionMessage(e))
  })
}

# 运行各个分析
run_script("scripts/China village insurance take up.r")
run_script("scripts/Test.r")
run_script("scripts/ECON 124 Empirical Project first try.r")

message(">>> Pipeline execution finished. Check 'output/' directory for results.")
