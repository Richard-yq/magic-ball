#!/bin/bash

# 切換到此檔案所在的資料夾
cd "$(dirname "$0")"

echo "=========================================="
echo "    正在為您準備 IG 抽獎神器 (Mac版)..."
echo "=========================================="
echo ""

# 檢查是否安裝了 Python 3
if ! command -v python3 &> /dev/null
then
    echo "錯誤: 找不到 Python 3。"
    echo "請先安裝 Python (建議從 python.org 下載)"
    exit
fi

echo "step 1. 正在檢查並安裝必要的元件 (selenium, pillow)..."
echo "        (第一次執行會跑比較久，請稍候)"
pip3 install selenium webdriver_manager pillow

echo ""
echo "step 2. 啟動程式..."
echo "=========================================="

# 執行 Python 程式
python3 ig_lottery_gui.py
