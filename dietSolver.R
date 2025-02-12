#@author: Myko Jefferson Javier
#@date: December 12, 2024
#@course: CMSC 150 -B4L

options(max.print=1000000)
options(digits = 5)

#Data
Foods = c("Frozen Broccoli", "Raw Carrots", "Raw Celery", "Frozen Corn", "Raw Iceberg Lettuce",
          "Raw Sweet Peppers", "Baked Potatoes", "Tofu","Roasted Chicken","Spaghetti W/ Sauce",
          "Ripe & Raw Red Tomato","Raw Apple W/Skin", "Banana","Grapes", "Fresh Raw Kiwifruit",
          "Oranges","Bagels","Wheat Bread","White Bread","Oatmeal Cookies","Apple Pie",
          "Chocolate Chip Cookies","Regular Butter", "Cheddar Cheese", "3.3% Whole Fat Milk",
          "2% Lowfat Milk","Skim Milk","Poached Eggs","Scrambled Eggs","Bologna Turkey", "Frankfurter Beef",
          "Extralean Sliced Ham","Pork Kielbasa","Cap'N Crunch", "Cheerios","Kellogg'S Corn Flakes", 
          "Kellogg'S Raisin Bran","Rice Krispies","Special K","Oatmeal", "Choc Malt-O-Meal","Pizza W/Pepperoni",
          "Taco","Hamburger W/Toppings","Hotdog Plain", "Couscous","White Rice", "Macaroni Ckd","Peanut Butter",
          "Pork","Sardines in Oil","White Tuna in Water", "Air-Popped Popcorn","Potato Chips Bbqflvr","Pretzels",
          "Tortilla Chip", "Chicken noodle Soup", "Split Pea & Hamsoup","Vegetbeef Soup","Neweng Clam Chowder",
          "Tomato Soup","New E Clamchwd,W/Mlk","Crm Mshrm Soup,W/Mlk", "Beanbacn Soup,W/Watr")

InfoTable = matrix(c(0.16,73.8,0,0.8,68.2,13.6,8.5,8,5867.4,160.2,159,2.3,
                     0.07,23.7,0,0.1,19.2,5.6,1.6,0.6,15471,5.1,14.9,0.3,
                     0.04,6.4,0,0.1,34.8,1.5,0.7,0.3,53.6,2.8,16,0.2,
                     0.18,72.2,0,0.6,2.5,17.1,2,2.5,106.6,5.2,3.3,0.3,
                     0.02,2.6,0,0,1.8,0.4,0.3,0.2,66,0.8,3.8,0.1,
                     0.53,20,0,0.1,1.5,4.8,1.3,0.7,467.7,66.1,6.7,0.3,
                     0.06,171.5,0,0.2,15.2,39.9,3.2,3.7,0,15.6,22.7,4.3,
                     0.31,88.2,0,5.5,8.1,2.2,1.4,9.4,98.6,0.1,121.8,6.2,
                     0.84,277.4,129.9,10.8,125.6,0,0,42.2,77.4,0,21.9,1.8,
                     0.78,358.2,0,12.3,1237.1,58.3,11.6,8.2,3055.2,27.9,80.2,2.3,
                     0.27,25.8,0,0.4,11.1,5.7,1.4,1,766.3,23.5,6.2,0.6,
                     0.24,81.4,0,0.5,0,21,3.7,0.3,73.1,7.9,9.7,0.2,
                     0.15,104.9,0,0.5,1.1,26.7,2.7,1.2,92.3,10.4,6.8,0.4,
                     0.32,15.1,0,0.1,0.5,4.1,0.2,0.2,24,1,3.4,0.1,
                     0.49,46.4,0,0.3,3.8,11.3,2.6,0.8,133,74.5,19.8,0.3,
                     0.15,61.6,0,0.2,0,15.4,3.1,1.2,268.6,69.7,52.4,0.1,
                     0.16,78,0,0.5,151.4,15.1,0.6,3,0,0,21,1,
                     0.05,65,0,1,134.5,12.4,1.3,2.2,0,0,10.8,0.7,
                     0.06,65,0,1,132.5,11.8,1.1,2.3,0,0,26.2,0.8,
                     0.09,81,0,3.3,68.9,12.4,0.6,1.1,2.9,0.1,6.7,0.5,
                     0.16,67.2,0,3.1,75.4,9.6,0.5,0.5,35.2,0.9,3.1,0.1,
                     0.03,78.1,5.1,4.5,57.8,9.3,0,0.9,101.8,0,6.2,0.4,
                     0.05,35.8,10.9,4.1,41.3,0,0,0,152.9,0,1.2,0,
                     0.25,112.7,29.4,9.3,173.7,0.4,0,7,296.5,0,202,0.2,
                     0.16,149.9,33.2,8.1,119.6,11.4,0,8,307.4,2.3,291.3,0.1,
                     0.23,121.2,18.3,4.7,121.8, 11.7,0,8.1,500.2,2.3,296.7,0.1,
                     0.13,85.5,4.4,0.4,126.2,11.9,0,8.4,499.8,2.4,302.3,0.1,
                     0.08,74.5,211.5,5,140,0.6,0,6.2,316,0,24.5,0.7,
                     0.11,99.6,211.2,7.3,168,1.3,0,6.7,409.2,0.1,42.6,0.7,
                     0.15,56.4,28.1,4.3,248.9,0.3,0,3.9,0,0,23.8,0.4,
                     0.27,141.8,27.4,12.8,461.7,0.8,0,5.4,0,10.8,9,0.6,
                     0.33,37.1,13.3,1.4,405.1,0.3,0,5.5,0,7.4,2,0.2,
                     0.15,80.6,17.4,7.1,279.8,0.6,0,3.4,0,5.5,11.4,0.4,
                     0.31,119.6,0,2.6,213.3,23,0.5,1.4,40.6,0,4.8,7.5,
                     0.28,111,0,1.8,307.6,19.6,2,4.3,1252.2,15.1,48.6,4.5,
                     0.28,110.5,0,0.1,290.5,24.5,0.7,2.3,1252.2,15.1,0.9,1.8,
                     0.34,115.1,0,0.7,204.4,27.9,4,4,1250.2,0,12.9,16.8,
                     0.32,112.2,0,0.2,340.8,24.8,0.4,1.9,1252.2,15.1,4,1.8,
                     0.38,110.8,0,0.1,265.5,21.3,0.7,5.6,1252.2,15.1,8.2,4.5,
                     0.82,145.1,0,2.3,2.3,25.3,4,6.1,37.4,0,18.7,1.6,
                     0.52,607.2,0,1.5,16.5,128.2,0,17.3,0,0,23.1,47.2,
                     0.44,181,14.2,7,267,19.9,0,10.1,281.9,1.6,64.6,0.9,
                     0.59,369.4,56.4,20.6,802,26.7,0,20.7,855,2.2,220.6,2.4,
                     0.83,275,42.8,10.2,563.9,32.7,0,13.6,126.3,2.6,51.4,2.5,
                     0.31,242.1,44.1,14.5,670.3,18,0,10.4,0,0.1,23.5,2.3,
                     0.39,100.8,0,0.1,4.5,20.9,1.3,3.4,0,0,7.2,0.3,
                     0.08,102.7,0,0.2,0.8,22.3,0.3,2.1,0,0,7.9,0.9,
                     0.17,98.7,0,0.5,0.7,19.8,0.9,3.3,0,0,4.9,1,
                     0.07,188.5,0,16,155.5,6.9,2.1,7.7,0,0,13.1,0.6,
                     0.81,710.8,105.1,72.2,38.4,0,0,13.8,14.7,0,59.9,0.4,
                     0.45,49.9,34.1,2.7,121.2,0,0,5.9,53.8,0,91.7,0.7,
                     0.69,115.6,35.7,2.1,333.2,0,0,22.7,68,0,3.4,0.5,
                     0.04,108.3,0,1.2,1.1,22.1,4.3,3.4,55.6,0,2.8,0.8,
                     0.22,139.2,0,9.2,212.6,15,1.2,2.2,61.5,9.6,14.2,0.5,
                     0.12,108,0,1,486.2,22.5,0.9,2.6,0,0,10.2,1.2,
                     0.19,142,0,7.4,149.7,17.8,1.8,2,55.6,0,43.7,0.4,
                     0.39,150.1,12.3,4.6,1862.2,18.7,1.5,7.9,1308.7,0,27.1,1.5,
                     0.67,184.8,7.2,4,964.8,26.8,4.1,11.1,4872,7,33.6,2.1,
                     0.71,158.1,10,3.8,1915.1,20.4,4,11.2,3785.1,4.8,32.6,2.2,
                     0.75,175.7,10,5,1864.9,21.8,1.5,10.9,20.1,4.8,82.8,2.8,
                     0.39,170.7,0,3.8,1744.4,33.2,1,4.1,1393,133,27.6,3.5,
                     0.99,163.7,22.3,6.6,992,16.6,1.5,9.5,163.7,3.5,186,1.5,
                     0.65,203.4,19.8,13.6,1076.3,15,0.5,6.1,153.8,2.2,178.6,0.6,
                     0.67,172,2.5,5.9,951.3,22.8,8.6,7.9,888,1.5,81,2), nrow = 64, byrow = TRUE)

#Functions

tableauSetUp = function(selected){
  n = length(selected)
  
  if(n == 1){
    return(NULL) #Cannot Set up tableau with 1 input
  }
  
  Cost = InfoTable[selected,1]
  
  #Minimization Variable
  Cost = append(Cost,0)
  
  NutritionTableau = InfoTable[selected,(2:12)]
  InitialTableau = cbind(NutritionTableau, NutritionTableau *-1)
  maxConst = c(2250,300,65,2400,300,100,100,50000,20000,1600,30)
  minConst = c(2000,0,0,0,0,25,50,5000,50,800,10)
  
  nutrientsConstraints = c(minConst *-1, maxConst)
  InitialTableau = rbind(InitialTableau, nutrientsConstraints)
  
  minServingConst = diag(x=1, nrow =(n+1), ncol = n)
  maxServingConst = diag(x=-1, nrow =n)
  maxServingConst = rbind(maxServingConst, replicate(n,10))
  
  Slacks = diag(x = 1, nrow = (n+1), ncol = n)
  
  P = c(1:length(selected)-1)*0
  P = append(P,1)
  P
  
  InitialTableau = cbind(InitialTableau, minServingConst, maxServingConst, Slacks, P, Cost)
  
  return(InitialTableau)
}

Simplex = function(selected){
  
  #Maximum Number of Iterations
  MAX_ITERATION = 1000
  
  
  tableau = tableauSetUp(selected)
  
  if(is.null(tableau)){
    return(NULL) #No Tableau was set-up (can't perform simplex)
  }
  #Get the row and column Sizes
  rowSize = nrow(tableau)
  colSize = ncol(tableau)
  
  #Labels
  slackLabels = c(paste("s", 1:(colSize - length(selected) - 2), sep = ""))
  varLabels = c(paste("x", 1:length(selected), sep = ""))
  Labels = append(slackLabels, varLabels)
  Labels = append(Labels, c("Z", "RHS"))
  Labels
  
  colnames(tableau) = Labels
  rownames(tableau) = NULL
  
  #Set up the list of matrices to store the tableau and basicSol per Iteration
  basicSolList = list(tableau[rowSize, -c(colSize-1)]) #stores the first basicSolution Iteration 0;
  tableauList = list(tableau) #stores the initial tableau
  iterationCount = 1 #Variable to keep track of number of iterations
  
  #Loop for Simplex Process
  while(TRUE){
    if(iterationCount > MAX_ITERATION){
      return(NULL)
    }
    pivotCol = which.min(tableau[rowSize, ]) #Gets the index of the minimum element in the last row
    if(tableau[rowSize,pivotCol] >= 0){ #Checks if it's a positive number
      break #If it is, end the loop (Computation Done).
    }else{ #If it is still a negative number, proceed with computations
      testRatio = ifelse((tableau[, pivotCol] == 0), -1, (tableau[, colSize])/(tableau[, pivotCol])) #Gets the test ratio
      ratioIndex = order(testRatio, decreasing = FALSE) #Arranges the indexes of the test ratio's in increasing order
      for(i in 1:rowSize){ #Loop for getting the pivotRow
        pivotRow = ratioIndex[i]
        if(testRatio[pivotRow] > 0){ #Gets the minimum positive test ratio
          break
        }
      }
      pivotElement = tableau[pivotRow, pivotCol] #Sets the pivot Element
      if(pivotElement == 0){ #Checks if PE is equal to 0
        return(NULL) #Stops execution and prints the prompt
      }else{ #If PE is not 0, proceed with the computation
        tableau[pivotRow,] = tableau[pivotRow,]/pivotElement #Normalize the pivotRow (dividing it with the PE)
        nPR = tableau[pivotRow, ] #Gets the normalized pivot row
        
        #Loop for eliminating rows (Gauss-Jordan)
        for(j in 1:rowSize){
          if(j == pivotRow){
            next
          }else{
            mult = tableau[j,pivotCol]
            normalizedRow = mult * nPR
            tableau[j,] = tableau[j,] - normalizedRow
          }
        }
      }
    }
    #Getting the Basic Solution
    basicSolution = tableau[rowSize, -c(colSize-1)] #Gets the last row of the tableau, excluding the colSize-1 column
    #Store the nth iteration's tableau and basicSolution to their respective list holder
    iterationCount = iterationCount + 1 #increment counter
    basicSolList[[iterationCount]] = basicSolution
    tableauList[[iterationCount]] = round(tableau,5)
    solution = basicSolution[colSize-1]
  }
  
  #Getting the servings
  ServingVec = basicSolution[((colSize-1)-(rowSize-1)):(colSize-2)]
  optIndex = c()
  optServings = c()
  for(i in 1:length(ServingVec)){
    if(ServingVec[i] > 0){
      optIndex = append(optIndex, selected[i])
      optServings = append(optServings, formatC(ServingVec[i]))
    }
  }
  
  #Optimized Menu
  optMenu = cbind(Foods[optIndex], optServings, InfoTable[optIndex,1])
  colnames(optMenu) = c("Foods", "Servings", "Cost")
  
  return(list(tableauList = tableauList, basicSolList = basicSolList, numIteration = iterationCount, Z = solution, menu = optMenu))
}
