# setup.R
# 自动安装所需的 R 包

required_packages <- c(
  "readstata13",
  "Matrix",
  "gamlr",
  "dplyr",
  "vtable",
  "ranger",
  "ggplot2",
  "stargazer",
  "kableExtra",
  "knitr"
)

# 检查并安装缺失的包
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(paste("Package already installed:", pkg))
  }
}

# 确保 output 目录存在
if (!dir.exists("output")) {
  dir.create("output")
  message("Created 'output' directory.")
}

message("Setup complete.")
