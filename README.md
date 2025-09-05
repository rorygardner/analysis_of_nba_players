
# Analysis of NBA Players
This project analyzes advanced player success metrics in professional basketball using a dataset spanning from 1947 to the present (sourced from Kaggle). The goal is to identify the key performance factors that drive player success and explore how these vary by player position.

## Key components: 
- Data cleaning and filtering to focus on NBA seasons post-1990 to reduce missing data bias.

- Application of principal component analysis (PCA) to reduce dimensionality and detect main performance dimensions.

- Visualization through biplots, scree plots, and correlation heatmaps to interpret variable contributions and relationships.

- Development of a linear regression model to predict Player Efficiency Rating (PER) in seasons where it was not originally recorded.

- Comparison of performance metrics across player positions highlighting strengths typical of centers, guards, and forwards.

- This analysis combines statistical rigor and sports domain insights to provide a data-driven understanding of basketball player performance metrics.

## Organization: 

- **analysis_of_nba_players**\
    Comprehensive report detailing the statistical exploration of NBA player performance metrics, including principal component analysis, regression modeling, and positional performance comparisons.

- **nba.R**\
    R script containing all data processing, analysis, PCA, visualization, and modeling code used to uncover key basketball player success factors from the NBA dataset.

- **data**\
    Folder containing the raw NBA player statistics dataset used for analysis, sourced from Kaggle, with relevant advanced metrics and player information.


## Data: 
The data in this project was found from Sumitro Datta on kaggle at:\
https://www.kaggle.com/datasets/sumitrodatta/nba-aba-baa-stats?resource=download 


