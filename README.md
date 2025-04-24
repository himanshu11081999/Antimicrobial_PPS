# Antimicrobial_PPS
The Antimicrobial Point Prevalence Survey (PPS) Dashboard is a robust R Shiny application designed to streamline data collection, automate analysis, and enhance antimicrobial stewardship programs in hospitals. It features a user-friendly interactive form for data entry, real-time analytics, and seamless integration with Google Sheets and Looker Studio for live reporting.

# Key Features
Interactive Data Entry Form
Structured and intuitive form for hospital staff to record patient-level antimicrobial use and stewardship indicators.

Automated Data Sync
Entries are stored in Google Sheets, enabling multi-user access and automatic updates.

Real-time Analysis
Visual dashboards that calculate key indicators:

% of patients on antimicrobials

Indication types (evidence based, empirical)

Embedded Google Looker Studio
Embedded Looker Studio dashboard allows live visualization of hospital-wide antimicrobial usage trends across wards, specialties, and time periods.

# Outputs and KPIs
Antimicrobial Use (%)

Indication Distribution

AWaRe Category Use (WHO classification)

Ward-wise Comparison

# Embedded Visualization Example

![image](https://github.com/user-attachments/assets/9121c17e-a465-438c-a226-5f1bd1f324e4)
![image](https://github.com/user-attachments/assets/206f9fb1-dd62-4d0f-95e8-8934b9cef3e8)
![image](https://github.com/user-attachments/assets/13660121-9227-4710-8349-0dfa814fc5b8)


# Technologies Used
R / Shiny – for form UI and reactive backend

conditional panel for reducing the complexity of the form

googlesheets4 – for live sync with Google Sheets

dplyr, tidyr, ggplot2 – for data wrangling and visualization

Looker Studio – for real-time embedded dashboards
