import warnings
warnings.simplefilter(action='ignore', category=DeprecationWarning) # This is to suppress the annoying Pandas warning upon importing.

import sys
import os
import json
import requests
import pandas as pd
from datetime import datetime

# Make sure you connected to the university staff VPN before running this!

def main():
    sys.stdin.reconfigure(encoding="utf-8")
    sys.stdout.reconfigure(encoding="utf-8")

    # 1. Format a path to the output directory...
    path_to_script_dir = os.path.dirname(os.path.abspath(__file__))
    current_time = datetime.now().strftime("%y-%m-%d_%H-%M-%S") # ...based on current time.
    output_path = os.path.join(path_to_script_dir, "..", "data", "pubs", "publications", current_time)
 
    # 2. Make the directory.
    os.makedirs(output_path)

    # 3. Get the Converis URLs to make API calls to.
    url_file = os.path.join(path_to_script_dir, "api.txt")
    with open(url_file) as file:
        url_list = [line.strip() for line in file]

    # 4. Access the API and collect the results in a single DataFrame.
    dfs = [] # We'll first aggregate everything here.

    for url in url_list:
        print(f"working on {url}")
        response = requests.get(url)  
        content = json.loads(response.text)
        publications_by_year = pd.DataFrame(content["data"])

        # NB! If you want, you could filter the results here using Pandas.
        # For now, let's just keep it as it is.

        dfs.append(publications_by_year)
    
    publications = pd.concat(dfs, ignore_index=True, sort=False)

    # 5. Save it as a .csv file.
    publications.to_csv(os.path.join(output_path, "publications.csv"))

if __name__ == "__main__":
    main()
