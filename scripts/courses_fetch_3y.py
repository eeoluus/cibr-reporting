import warnings
warnings.simplefilter(action='ignore', category=DeprecationWarning) # This is to suppress the annoying Pandas warning upon importing.

import sys
import os
import asyncio
from playwright.async_api import async_playwright
import pandas as pd
from datetime import datetime

# Make sure you connected to the university staff VPN before running this!
# Unfortunately the API offers only the last three years' data.
# Ask from HelpJYU if you need more.

async def main():
    sys.stdin.reconfigure(encoding="utf-8")
    sys.stdout.reconfigure(encoding="utf-8")
    
    # 1. Format a path to the output directory...
    path_to_script_dir = os.path.dirname(os.path.abspath(__file__))
    current_time = datetime.now().strftime("%y-%m-%d_%H-%M-%S") # ...based on current time.
    output_path = os.path.join(path_to_script_dir, "courses", current_time)

    # 2. Make the directory.
    os.makedirs(output_path)

    # 3. Scrape the table from the Sisu page.
    async with async_playwright() as pw:
        browser = await pw.chromium.launch()
        page = await browser.new_page()
        await page.goto(
            "https://sisu.pages.kopla.jyu.fi/teaching-reports/", # Page is huge, long timeout needed.
            timeout=60000 
        ) 
        [psy_courses] = await page.locator("#jy-ORG-62").all_inner_texts() # The organisation identifier
                                                                          # belongs to the Dept. of Psychology.
    # 4. Collect contents of the table in a DataFrame.
    # We first split by new lines and then by tabs.
    header, *data = [line.split("\t") for line in psy_courses.splitlines()] 

    df = pd.DataFrame(data, columns=header) # The first row is the header.

    # 5. Save it as a .csv file.
    df.to_csv(os.path.join(output_path, "courses.csv"), index=False)

if __name__== '__main__':
    asyncio.run(main())
