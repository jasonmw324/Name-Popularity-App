
# Name Popularity App

An interactive **Shiny** web app that visualizes baby name popularity across U.S. states and over time.

🔗 **Live App:** [View the app online](https://jasonmw324.shinyapps.io/namepopularityapp/)

---

## 🚀 Features
- View the **most popular baby names** in a selected U.S. state (or nationwide) for a given year and sex.  
- **Track name popularity** of up to eight specific names across a range of years.  
- Compare naming trends **between two states** side-by-side.  
- Interactive charts and tables built with Shiny and Plotly.

---

## 🧪 Data & Methods
- Uses publicly available baby name data from the **U.S. Social Security Administration (SSA)**.  
- Data includes: name, year, state, sex, and number of births.  
- The app provides visual comparisons and rankings using R’s data manipulation and visualization packages.

---

## 🔧 How to Run Locally

1. Click the green **Code → Download ZIP** button on this repository’s GitHub page.  
2. Extract the downloaded ZIP file to your computer.  
3. Open the folder in **RStudio**.  
4. Open the file `app.R` and click **Run App** in the top right corner.  

That’s it — no extra setup required.

---

## 🗂 Repository Structure

encryptzor/
├── main.py                   # Application entry point
├── core/                     # Core functionality
│   ├── auth.py              # Authentication & master password
│   ├── encryption.py        # Kryptos cipher + AES wrapper
│   ├── vault.py             # Encrypted storage management
│   └── utils.py             # Crypto utilities & helpers
├── gui/                      # User interface
│   ├── login_window.py      # Login interface
│   ├── main_window.py       # Main password manager UI
│   └── components.py        # Reusable UI components
├── tests/                    # Unit tests
│   ├── test_auth.py
│   ├── test_encryption.py
│   └── test_vault.py
├── docs/                     # Documentation
│   ├── KRYPTOS_CIPHER.md    # Cipher implementation details
│   └── SECURITY_MODEL.md    # Security architecture
├── data/                     # User data (created at runtime)
│   ├── vault.enc            # Encrypted password vault
│   └── config.json          # Non-sensitive configuration
└── requirements.txt          # Python dependencies








