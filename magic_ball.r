球魔方 <- function(n = 100000) {
    jvzen <- matrix(c(
        1, 2, 3,  # 第一橫排
        4, 5, 6,  # 第二橫排
        7, 8, 9,  # 第三橫排
        1, 4, 7,  # 第一直排
        2, 5, 8,  # 第二直排
        3, 6, 9,  # 第三直排
        1, 5, 9,  # 對角線1
        3, 5, 7   # 對角線2
    ), nrow = 8, ncol = 3, byrow = TRUE)
    
    # 初始化計數器
    count <- 0
    count2 <- 0
    
    # 顯示遊戲開始訊息
    cat("==================================================\n")
    cat("       球魔方遊戲模擬開始\n")
    cat("==================================================\n")
    cat(sprintf("模擬次數: %d 次\n", n))
    cat("--------------------------------------------------\n\n")
    
    # 進行n次模擬
    for (i in 1:n) {
        cat("==================================================\n")
        cat(sprintf("%d\n", i))
        
        # 電腦隨機選擇一組中獎連線
        roll <- jvzen[sample(1:8, 1), ]
        
        # 顯示預測中獎
        cat("預測中獎:\n")
        roll_mat <- rep(NA, 9)
        roll_mat[roll] <- "O"
        mat <- matrix(roll_mat, nrow = 3, byrow = TRUE)
        print(mat)
        
        # 玩家的三顆球隨機落在9個位置中的3個
        roll2 <- sample(1:9, 3)
        
        # 顯示玩家連線
        cat("玩家連線:\n")
        roll2_mat <- rep(NA, 9)
        roll2_mat[roll2] <- "O"
        mat2 <- matrix(roll2_mat, nrow = 3, byrow = TRUE)
        print(mat2)
        
        # 判斷玩家位置是否與中獎連線完全相同
        if (setequal(roll2, roll)) {
            cat("結果: 中獎\n\n")
            count <- count + 1
        } else {
            cat("結果: 未中獎\n\n")
            count2 <- count2 + 1
        }
    }
    
    # # ========== 輸出 (1): 未中獎和中獎的說明 ==========
    # cat("【遊戲規則說明】\n")
    # cat("1. 遊戲使用3x3的井字格球盤（共9個位置）\n")
    # cat("2. 電腦會隨機指定一組連線（橫、直、斜共8種可能）\n")
    # cat("3. 玩家的三顆球會隨機落在9個位置中的3個\n")
    # cat("4. 如果玩家的三顆球恰好落在中獎連線上，則中獎\n")
    # cat("5. 否則為未中獎\n\n")
    
    # 顯示井字格位置示意圖
    cat("球盤位置示意圖：\n")
    cat("  1 | 2 | 3\n")
    cat("  ---------\n")
    cat("  4 | 5 | 6\n")
    cat("  ---------\n")
    cat("  7 | 8 | 9\n\n")
    
    # ========== 輸出 (2): 中獎數量統計 ==========
    cat("==================================================\n")
    cat("       模擬結果統計\n")
    cat("==================================================\n")
    cat(sprintf("總模擬次數: %d 次\n", n))
    cat("--------------------------------------------------\n")
    cat(sprintf("中獎次數:   %d 次\n", count))
    cat(sprintf("未中獎次數: %d 次\n", count2))
    cat("--------------------------------------------------\n")
    cat(sprintf("中獎機率:   %.4f%% (%.2f/100)\n", 
                (count/n)*100, 
                (count/n)*100))
    cat(sprintf("理論機率:   %.4f%% (1 ÷ C(9,3) = 1/84)\n", 
                (1/choose(9,3))*100))
    cat("==================================================\n\n")
    
    # ========== 輸出 (3): 統計長條圖 ==========
    # 準備繪圖資料
    result <- c(count2, count)
    labels_text <- c("Not Win", "Win")
    colors <- c("red", "green")  # 紅色表示未中獎，藍綠色表示中獎
    
    # 輸出為 PDF 檔案
    pdf("magic_ball_result.pdf", width = 10, height = 7)
    
    # 設定繪圖參數
    par(mar = c(5, 5, 4, 2))
    
    # 繪製長條圖
    barplot_result <- barplot(
        result,
        names.arg = labels_text,
        col = colors,
        main = sprintf("Magic Ball Statistics\n(Simulations: %s)", 
                       format(n, big.mark = ",")),
        xlab = "Result",
        ylab = "Count",
        ylim = c(0, max(result) * 1.15),
        border = "white",
        las = 1,
        cex.main = 1.5,
        cex.lab = 1.2,
        cex.names = 1.2
    )
    
    # 在長條上方顯示數值和百分比
    text(
        x = barplot_result,
        y = result,
        labels = sprintf("%s\n(%.2f%%)", 
                         format(result, big.mark = ","),
                         (result/n)*100),
        pos = 3,
        cex = 1.2,
        font = 2
    )
    
    # 添加網格線
    grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
    
    # 重新繪製長條圖以覆蓋網格線
    barplot(
        result,
        names.arg = labels_text,
        col = colors,
        border = "white",
        add = TRUE,
        axes = FALSE
    )
    
    # 添加圖例
    legend("topright",
           legend = c(sprintf("Not Win: %s times", format(count2, big.mark = ",")),
                      sprintf("Win: %s times", format(count, big.mark = ","))),
           fill = colors,
           border = "white",
           bty = "n",
           cex = 1.2)
    
    # 關閉 PDF 設備
    dev.off()
    
    cat("\n圖表已儲存至: magic_ball_result.pdf\n")
    
    # 返回統計結果
    invisible(list(
        模擬次數 = n,
        中獎次數 = count,
        未中獎次數 = count2,
        中獎機率 = count/n,
        統計結果 = result
    ))
}

# ============================================================================
# 執行函數 - 模擬十萬次
# ============================================================================
球魔方(100000)






