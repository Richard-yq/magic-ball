@echo off
title IG 抽獎神器啟動中...
echo ==========================================
echo     正在為您準備 IG 抽獎神器...
echo ==========================================
echo.

echo step 1. 檢查必要的元件...
pip install selenium webdriver_manager pillow

echo.
echo step 2. 啟動程式...
echo ==========================================

python ig_lottery_gui.py

if %errorlevel% neq 0 (
    echo.
    echo 程式發生錯誤，請截圖給開發者。
    pause
)
