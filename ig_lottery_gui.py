import tkinter as tk
from tkinter import messagebox, scrolledtext
from PIL import Image, ImageTk # Need to ensure PIL is available, or use os.startfile
import random
import threading
import time
import json
import re
import os
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from webdriver_manager.chrome import ChromeDriverManager

class IGLotteryApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Instagram 抽獎神器 (附截圖版)")
        self.root.geometry("700x850")
        self.bg_color = "#f0f0f0"
        self.root.configure(bg=self.bg_color)
        
        # --- UI ---
        tk.Label(root, text="Instagram 抽獎神器 (附截圖功能)", font=("Arial", 18, "bold"), bg=self.bg_color).pack(pady=10)
        
        info = (
            "使用說明：\n"
            "1. 開啟瀏覽器並登入。\n"
            "2. 點擊抓取 (瞬間讀取)。\n"
            "3. 點擊抽獎時，**瀏覽器會自動跳轉**到該則留言並截圖。"
        )
        tk.Label(root, text=info, justify="left", bg="#e8e8e8", padx=10, pady=5).pack(fill="x", padx=20)

        # Inputs
        frame_input = tk.Frame(root, bg=self.bg_color)
        frame_input.pack(pady=10)
        tk.Label(frame_input, text="貼文網址:", bg=self.bg_color).pack(side="left")
        self.entry_url = tk.Entry(frame_input, width=50)
        self.entry_url.pack(side="left", padx=5)
        self.entry_url.insert(0, "https://www.instagram.com/p/DUAA8vdkf9D/")

        # Buttons
        frame_btns = tk.Frame(root, bg=self.bg_color)
        frame_btns.pack(pady=10)
        self.btn_open = tk.Button(frame_btns, text="1. 開啟瀏覽器", command=self.open_browser, bg="#2196F3", fg="white", font=("Arial", 10, "bold"), padx=10)
        self.btn_open.pack(side="left", padx=5)
        
        self.btn_fetch = tk.Button(frame_btns, text="2. 開始抓取", command=self.start_fetch, bg="#4CAF50", fg="white", font=("Arial", 10, "bold"), padx=10, state="disabled")
        self.btn_fetch.pack(side="left", padx=5)
        
        self.btn_draw = tk.Button(frame_btns, text="3. 抽獎並截圖", command=self.draw_winner, bg="#FF5722", fg="white", font=("Arial", 10, "bold"), padx=10, state="disabled")
        self.btn_draw.pack(side="left", padx=5)

        # Log
        self.txt_output = scrolledtext.ScrolledText(root, height=15)
        self.txt_output.pack(fill="both", expand=True, padx=20, pady=10)

        self.driver = None
        self.comments_list = []
        self.current_shortcode = ""

    def log(self, msg):
        now = datetime.now().strftime("%H:%M:%S")
        self.txt_output.insert(tk.END, f"[{now}] {msg}\n")
        self.txt_output.see(tk.END)

    def open_browser(self):
        thread = threading.Thread(target=self._open_browser_task)
        thread.start()

    def _open_browser_task(self):
        try:
            self.log("啟動瀏覽器中...")
            service = Service(ChromeDriverManager().install())
            options = webdriver.ChromeOptions()
            options.add_argument("--disable-notifications")
            options.add_argument("--start-maximized")
            self.driver = webdriver.Chrome(service=service, options=options)
            self.driver.get("https://www.instagram.com/accounts/login/")
            
            self.log("瀏覽器開啟成功。請手動登入 Instagram。")
            self.root.after(0, lambda: self.btn_open.config(state="disabled"))
            self.root.after(0, lambda: self.btn_fetch.config(state="normal"))
        except Exception as e:
            self.log(f"開啟失敗: {e}")

    def start_fetch(self):
        url = self.entry_url.get().strip()
        if not url: return
        self.btn_fetch.config(state="disabled")
        thread = threading.Thread(target=self._fetch_js_task, args=(url,))
        thread.start()

    def _fetch_js_task(self, url):
        try:
            self.log(f"準備抓取: {url}")
            if url not in self.driver.current_url:
                self.driver.get(url)
                time.sleep(3)

            # 1. Parse Shortcode
            match = re.search(r'(?:p|reel)/([^/?#&]+)', url)
            if match:
                self.current_shortcode = match.group(1)
                media_id = self.shortcode_to_mediaid(self.current_shortcode)
                self.log(f"Shortcode: {self.current_shortcode} / ID: {media_id}")
            else:
                self.log("網址格式錯誤")
                return

            # 2. JS Fetch
            self.log("開始下載留言...")
            self.comments_list = []
            next_cursor = ""
            count = 0
            has_next = True
            
            while has_next:
                api_url = f"/api/v1/media/{media_id}/comments/?can_support_threading=true&permalink_enabled=false"
                if next_cursor:
                    api_url += f"&min_id={next_cursor}"
                
                js_script = """
                    var url = arguments[0];
                    var callback = arguments[1];
                    fetch(url, {
                        headers: {'X-IG-App-ID': '936619743392459', 'X-Requested-With': 'XMLHttpRequest'}
                    })
                    .then(r => r.json()).then(d => callback(d))
                    .catch(e => callback({error: e.toString()}));
                """
                
                result = self.driver.execute_async_script(js_script, api_url)
                if not result: break
                if 'error' in result: break
                
                comments = result.get('comments', [])
                for c in comments:
                    user = c.get('user', {}).get('username', 'unknown')
                    text = c.get('text', '')
                    ts = c.get('created_at_utc', 0)
                    cid = c.get('pk') or c.get('id') # Capture Comment ID
                    dt = datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
                    
                    self.comments_list.append({
                        'owner': user,
                        'text': text,
                        'time': dt,
                        'ts': ts,
                        'id': cid
                    })
                
                count += len(comments)
                if count % 50 == 0: self.log(f"已下載 {count} 則...")
                
                next_cursor = result.get('next_min_id')
                if not next_cursor: has_next = False
                time.sleep(random.uniform(0.5, 1.5))

            self.comments_list.sort(key=lambda x: x['ts'])
            self.log(f"抓取完成！共 {len(self.comments_list)} 則。")
            self.root.after(0, lambda: self.btn_draw.config(state="normal"))
            self.root.after(0, lambda: self.btn_fetch.config(state="normal"))

        except Exception as e:
            self.log(f"錯誤: {e}")
            self.root.after(0, lambda: self.btn_fetch.config(state="normal"))

    def shortcode_to_mediaid(self, shortcode):
        alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
        media_id = 0
        for letter in shortcode:
            media_id = (media_id * 64) + alphabet.index(letter)
        return str(media_id)

    def draw_winner(self):
        if not self.comments_list: return
        
        # Don't block UI mostly, but we need driver interaction
        thread = threading.Thread(target=self._draw_and_screenshot)
        thread.start()

    def _draw_and_screenshot(self):
        try:
            self.log("正在抽獎...")
            time.sleep(1)
            winner = random.choice(self.comments_list)
            
            self.log(f"🎉 中獎者: {winner['owner']} 🎉")
            self.log("正在前往該留言進行截圖...")
            
            # Construct Permalink
            # https://www.instagram.com/p/{shortcode}/c/{comment_id}/
            if winner['id']:
                target_url = f"https://www.instagram.com/p/{self.current_shortcode}/c/{winner['id']}/"
                self.driver.get(target_url)
                
                # Wait for load and highlighting (IG usually highlights in background color or scolls to it)
                time.sleep(5) 
                
                # Try to clean up UI? (Close login banner if any?)
                # Just take full window shot
                filename = f"winner_{winner['owner']}_{int(time.time())}.png"
                self.driver.save_screenshot(filename)
                
                self.log(f"截圖已儲存為: {filename}")
                self.log("正在開啟圖片...")
                os.startfile(filename)
            else:
                self.log("無法獲取留言 ID，無法跳轉截圖。")

            # Display Text Result
            msg = (
                f"🎉 幸運得主 🎉\n\n"
                f"帳號: {winner['owner']}\n"
                f"時間: {winner['time']}\n"
                f"內容: {winner['text']}\n"
            )
            self.txt_output.insert(tk.END, "\n" + "-"*30 + "\n" + msg + "-"*30 + "\n")
            self.txt_output.see(tk.END)
            messagebox.showinfo("中獎", f"恭喜 {winner['owner']}！\n截圖已開啟。")
            
        except Exception as e:
            self.log(f"截圖與抽獎過程發生錯誤: {e}")

if __name__ == "__main__":
    root = tk.Tk()
    app = IGLotteryApp(root)
    root.mainloop()
