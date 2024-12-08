# CMSC-150-Project

![Alt text](www/Nutrivise_logo.png)

# Nutrivise <br/>

NutriVise is a diet solver application that uses linear programming (Simplex Method), to find the cheapest
and most nutritious combinations of food from a given selection. The program depends on a predetermined nutritional
data and constraints (provided in the Project Specifications), and computes for the cost of the most optimal diet.<br/>

## Built With:<br/>

> The program was developed using R and Shiny, along with some CSS and JS for styling and functionality.

## How Nutrivise Works<br/>

On the Diet Solver page, click on the checkboxes on the Food Selection panel in order to select some foods _(selected foods will be shown)_. Once you are ready, click the **Optimize** button to prompt the program to start with the Simplex Computation.

> \***_NOTE:_** not all food combinations will yield an answer, some of them are infeasible _(the program will show a prompt whenever a problem is infeasible)_

After clicking the Optimize button, if the food selection was solvable, the computer **minimum cost** along with the **optimized menu** will be shown. In order to see the _Initial and Per Iteration_ tableau and basic solution, just click the **View Result** button.

### About the Developer:<br/>

**_@author:_** Myko Jefferson Javier<br/>
**_BSCS Student @ UP los Banos_**<br/>
**_@github:_** [github.com/dawoo01](https://github.com/dawoo01)
