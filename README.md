
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fantasy

<!-- badges: start -->
<!-- badges: end -->

{fantasy} is a wrapper for the Fantasy Premier League API. It provides a
clean, intuitive set of functions to extract all the key data that is
available from the API, as well as enabling actions such as transfers
and team changes to be enacted programmatically.

After authenticating, the package provides a new class of object
(`team`) that takes care of all the various restrictions on a team, so
you can simply make transfers and substitutions and the package will
notify you if they are not valid (e.g. more than 3 players from 1 club,
not enough of a certain position in your team/starting XI, if a
transfer exceeds your budget etc.). The `<team>` class also enables
pretty printing of a team (see usage below).

It maintains a tidyverse-like style, with all functions returning
tibbles with readable column names. The package also provides helper
functions for obtaining data from other useful sources, such as
predicted team lineups from fantasyfootballscout.co.uk or relevant
tweets from twitter.

## Installation

You can install the development version of fantasy from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chrisbrownlie/fantasy")
```

## Motivation

There already exists at least two R packages that wrap the Fantasy
Premier League API:

-   [fplR](https://ewenme.github.io/fplr/)
-   [fplscrapR](https://wiscostret.github.io/fplscrapR/)

The reasons for building this new package are because the two packages
above:

-   Are not regularly maintained, and so may not work with the current
    (2022/23) version of the FPL API,
-   Do not implement authentication, so cannot be used to perform
    actions and retrieve private user information, or
-   Are not built using {httr2} and so do not benefit from some of its
    functionality.

{fantasy} aims to address all three of these restrictions.

## Usage

Below are some examples of how to use the key functions in the package.
See the package site for more detailed information and examples.

``` r
library(fantasy)
```

    #> ℹ Loading fantasy
    #> ℹ Enabling function caching...

``` r
# Get all fixtures from the current season
get_fixture_list()
#> # A tibble: 380 × 11
#>       id gameweek finished kickoff_time        minutes home_team home_…¹ home_…²
#>    <int>    <int> <lgl>    <dttm>                <int> <chr>       <int>   <int>
#>  1     1        1 TRUE     2022-08-05 00:00:00      90 CRY             0       3
#>  2     4        1 TRUE     2022-08-06 00:00:00      90 FUL             2       5
#>  3     2        1 TRUE     2022-08-06 00:00:00      90 BOU             2       2
#>  4     5        1 TRUE     2022-08-06 00:00:00      90 LEE             2       2
#>  5     7        1 TRUE     2022-08-06 00:00:00      90 NEW             2       2
#>  6     8        1 TRUE     2022-08-06 00:00:00      90 TOT             4       2
#>  7     3        1 TRUE     2022-08-06 00:00:00      90 EVE             0       4
#>  8     6        1 TRUE     2022-08-07 00:00:00      90 LEI             2       2
#>  9     9        1 TRUE     2022-08-07 00:00:00      90 MUN             1       2
#> 10    10        1 TRUE     2022-08-07 00:00:00      90 WHU             0       5
#> # … with 370 more rows, 3 more variables: away_team <chr>, away_score <int>,
#> #   away_difficulty <int>, and abbreviated variable names ¹​home_score,
#> #   ²​home_difficulty
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# Get stats from a particular fixture, by ID
get_fixture_stats(fixture_id = 2)
#> # A tibble: 42 × 5
#>    fixture_id stat         team  player       value
#>         <int> <chr>        <chr> <chr>        <int>
#>  1          2 goals_scored home  Moore            1
#>  2          2 goals_scored home  Lerma            1
#>  3          2 assists      home  Kelly            1
#>  4          2 yellow_cards home  Smith            1
#>  5          2 yellow_cards home  Pearson          1
#>  6          2 yellow_cards home  Billing          1
#>  7          2 yellow_cards away  Ings             1
#>  8          2 yellow_cards away  Douglas Luiz     1
#>  9          2 yellow_cards away  Ramsey           1
#> 10          2 saves        home  Travers          2
#> # … with 32 more rows
#> # ℹ Use `print(n = ...)` to see more rows

# Get all team info
get_teams()
#> # A tibble: 20 × 19
#>     code  draw team_id  loss name  played points posit…¹ short…² stren…³ unava…⁴
#>    <int> <int>   <int> <int> <chr>  <int>  <int>   <int> <chr>     <int> <lgl>  
#>  1     3     0       1     0 Arse…      0      0       0 ARS           4 FALSE  
#>  2     7     0       2     0 Asto…      0      0       0 AVL           3 FALSE  
#>  3    91     0       3     0 Bour…      0      0       0 BOU           2 FALSE  
#>  4    94     0       4     0 Bren…      0      0       0 BRE           3 FALSE  
#>  5    36     0       5     0 Brig…      0      0       0 BHA           3 FALSE  
#>  6     8     0       6     0 Chel…      0      0       0 CHE           4 FALSE  
#>  7    31     0       7     0 Crys…      0      0       0 CRY           3 FALSE  
#>  8    11     0       8     0 Ever…      0      0       0 EVE           3 FALSE  
#>  9    54     0       9     0 Fulh…      0      0       0 FUL           2 FALSE  
#> 10    13     0      10     0 Leic…      0      0       0 LEI           3 FALSE  
#> 11     2     0      11     0 Leeds      0      0       0 LEE           2 FALSE  
#> 12    14     0      12     0 Live…      0      0       0 LIV           5 FALSE  
#> 13    43     0      13     0 Man …      0      0       0 MCI           5 FALSE  
#> 14     1     0      14     0 Man …      0      0       0 MUN           4 FALSE  
#> 15     4     0      15     0 Newc…      0      0       0 NEW           3 FALSE  
#> 16    17     0      16     0 Nott…      0      0       0 NFO           2 FALSE  
#> 17    20     0      17     0 Sout…      0      0       0 SOU           3 FALSE  
#> 18     6     0      18     0 Spurs      0      0       0 TOT           4 FALSE  
#> 19    21     0      19     0 West…      0      0       0 WHU           3 FALSE  
#> 20    39     0      20     0 Wolv…      0      0       0 WOL           3 FALSE  
#> # … with 8 more variables: win <int>, strength_overall_home <int>,
#> #   strength_overall_away <int>, strength_attack_home <int>,
#> #   strength_attack_away <int>, strength_defence_home <int>,
#> #   strength_defence_away <int>, pulse_id <int>, and abbreviated variable names
#> #   ¹​position, ²​short_name, ³​strength, ⁴​unavailable
#> # ℹ Use `colnames()` to see all variable names

# Get all players info
get_players()
#> # A tibble: 584 × 56
#>       id name         known…¹ posit…²  team team_…³  cost points point…⁴ point…⁵
#>    <dbl> <chr>        <chr>   <chr>   <dbl>   <dbl> <dbl>  <dbl>   <dbl>   <dbl>
#>  1     1 Cédric Alve… Cédric  DEF         1       3   4.4      0       0     0  
#>  2     3 Granit Xhaka Xhaka   MID         1       3   5       12      14     7  
#>  3     4 Mohamed Eln… Elneny  MID         1       3   4.4      0       0     0  
#>  4     5 Rob Holding  Holding DEF         1       3   4.4      0       0     0  
#>  5     6 Thomas Part… Partey  MID         1       3   5        2       5     2.5
#>  6     7 Martin Ødeg… Ødegaa… MID         1       3   6.5      2       5     2.5
#>  7     8 Kieran Tier… Tierney DEF         1       3   5        1       2     1  
#>  8     9 Nicolas Pépé Pépé    MID         1       3   5.4      0       0     0  
#>  9    10 Benjamin Wh… White   DEF         1       3   4.5      1       6     3  
#> 10    11 Eddie Nketi… Nketiah FWD         1       3   6.9      1       2     1  
#> # … with 574 more rows, 46 more variables: cost_change_recent <dbl>,
#> #   cost_change_from_start <dbl>, form <dbl>, value_form <dbl>,
#> #   value_season <dbl>, selected_by_pct <dbl>, transfers_in_recent <dbl>,
#> #   transfers_in_total <dbl>, transfers_out_recent <dbl>,
#> #   transfers_out_total <dbl>, expected_points_this_week <dbl>,
#> #   expected_points_next_week <dbl>, minutes <dbl>, goals_scored <dbl>,
#> #   assists <dbl>, clean_sheets <dbl>, goals_conceded <dbl>, own_goals <dbl>, …
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# Get a particular player's season statistics
get_player_summary(player_id = 283)
#> # A tibble: 38 × 33
#>    element_id fixture_id round kickoff_time        at_home team_h team_…¹ team_a
#>         <dbl>      <int> <int> <dttm>              <lgl>   <chr>    <int> <chr> 
#>  1        283          4     1 2022-08-06 00:00:00 FALSE   FUL          2 LIV   
#>  2        283         16     2 2022-08-15 00:00:00 TRUE    LIV          1 CRY   
#>  3        283         27     3 2022-08-22 00:00:00 FALSE   MUN         NA LIV   
#>  4        283         36     4 2022-08-27 00:00:00 TRUE    LIV         NA BOU   
#>  5        283         48     5 2022-08-31 00:00:00 TRUE    LIV         NA NEW   
#>  6        283         55     6 2022-09-03 00:00:00 FALSE   EVE         NA LIV   
#>  7        283         67     7 2022-09-10 00:00:00 TRUE    LIV         NA WOL   
#>  8        283         74     8 2022-09-18 00:00:00 FALSE   CHE         NA LIV   
#>  9        283         87     9 2022-10-01 00:00:00 TRUE    LIV         NA BHA   
#> 10        283         91    10 2022-10-09 00:00:00 FALSE   ARS         NA LIV   
#> # … with 28 more rows, 25 more variables: team_a_score <int>,
#> #   total_points <int>, difficulty <int>, minutes <int>, goals_scored <int>,
#> #   assists <int>, clean_sheets <int>, goals_conceded <int>, own_goals <int>,
#> #   penalties_saved <int>, penalties_missed <int>, yellow_cards <int>,
#> #   red_cards <int>, saves <int>, bonus <int>, bps <int>, influence <chr>,
#> #   creativity <chr>, threat <chr>, ict_index <chr>, value <int>,
#> #   transfers_balance <int>, selected <int>, transfers_in <int>, …
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# Authenticate to allow restricted actions - see vignette for more information
authenticate()
#> ℹ Sending login request for user chris.brownlie@hotmail.co.uk...
#> ✔ Login successful, overwriting existing cookie...
#> ✔ Authentication successful!

# See your team
get_my_team()
#> ℹ Team selection:
#> GKP: 15-Ramsdale
#> DEF: 430-Dier; 299-Walker; 10-White; 280-Van Dijk
#> MID: 283-Salah (C); 428-Son (VC); 305-Grealish; 370-S.Longstaff; 465-Bowen
#> FWD: 255-Vardy
#> (Bench): 398-Henderson; 199-Tarkowski; 391-Surridge; 166-Edouard

# Swap two players in your team
# - {fantasy} will automatically handle any restrictions or reordering of your team
get_my_team() |>
  team_substitute(p1 = 465, p2 = 166)
#> ℹ Team selection:
#> GKP: 15-Ramsdale
#> DEF: 430-Dier; 299-Walker; 10-White; 280-Van Dijk
#> MID: 283-Salah (C); 428-Son (VC); 305-Grealish; 370-S.Longstaff
#> FWD: 166-Edouard; 255-Vardy
#> (Bench): 398-Henderson; 199-Tarkowski; 465-Bowen; 391-Surridge

# Change the captain or vice captain
get_my_team() |>
  assign_role(pid = 255, role = "c") |>
  assign_role(pid = 15, role = "vc")
#> ℹ Team selection:
#> GKP: 15-Ramsdale (VC)
#> DEF: 430-Dier; 299-Walker; 10-White; 280-Van Dijk
#> MID: 283-Salah; 428-Son; 305-Grealish; 370-S.Longstaff; 465-Bowen
#> FWD: 255-Vardy (C)
#> (Bench): 398-Henderson; 199-Tarkowski; 391-Surridge; 166-Edouard


### Non-FPL API functions
# Get predicted lineups from fantasyfootballscout.co.uk
get_predicted_lineups()
#> # A tibble: 220 × 3
#>    team    selection player           
#>    <chr>       <dbl> <chr>            
#>  1 Arsenal         1 Ramsdale         
#>  2 Arsenal         2 White            
#>  3 Arsenal         3 Saliba           
#>  4 Arsenal         4 Gabriel Magalhães
#>  5 Arsenal         5 Zinchenko        
#>  6 Arsenal         6 Odegaard         
#>  7 Arsenal         7 Partey           
#>  8 Arsenal         8 Xhaka            
#>  9 Arsenal         9 Saka             
#> 10 Arsenal        10 Jesus            
#> # … with 210 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```
