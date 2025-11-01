
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
```markdown
NamePopularityApp/
├── functions/            # Functions used in the app
│   ├── .RData
│   ├── .Rhistory
│   ├── NameAppFunctions.R    # R file with custom functions used in app         
├── rsconnect/shinyapps.io/jasonmw324/
├── .RData                      
├── .RDataTmp                
├── .Rhistory  
├── NationalData             # National Data used in App
├── README.md                
├── StateData                # State Data used in App
├── app.R                    # Main Shiny App File
└── deployApp.R              # Script to deploy app to shinyapps.io











