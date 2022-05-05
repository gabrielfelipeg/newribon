library(lpSolveAPI)  # lpSolveAPI (http://lpsolve.sourceforge.net/5.5/R.htm)

# Perform optimization and get ouputs
#
# Args:
#   insts: Instance usage
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: Maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: Maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: Maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: Maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: Maximum usage of percentage of all-upfront 3-year instances
#   printProg: Whether printing progress or not
#
# Returns
#   List that contains all the results of optimization
#     updateUps: 
#       Updated upfront instances
#     instsTotal: 
#       Total usage of instances
#     instsODM.base: 
#       Instance usage of On-demand per month (w/o Optimization)
#     instsNM1.base: 
#       Instance usage of No-upfront 1-Year per month (w/o Optimization)
#     instsPM1.base: 
#       Instance usage of Partial-upfront 1-Year per month (w/o Optimization)
#     instsPM3.base: 
#       Instance usage of Partial-upfront 3-Year per month (w/o Optimization)
#     instsAM1.base: 
#       Instance usage of All-upfront 1-Year per month (w/o Optimization)
#     instsAM3.base: 
#       Instance usage of All-upfront 3-Year per month (w/o Optimization)
#     instsODM.opt: 
#       Instance usage of On-demand per month (with Optimization)
#     instsNU1.opt: 
#       Upfront instances of No-upfront 1-Year (with Optimization)
#     instsPU1.opt: 
#       Upfront instances of Partial-upfront 1-Year (with Optimization)
#     instsPU3.opt: 
#       Upfront instances of Partial-upfront 3-Year (with Optimization)
#     instsAU1.opt: 
#       Upfront instances of All-upfront 1-Year (with Optimization)
#     instsAU3.opt: 
#       Upfront instances of All-upfront 3-Year (with Optimization)
#     instsNM1.opt: 
#       Instance usage of No-upfront 1-Year per month (with Optimization)
#     instsPM1.opt: 
#       Instance usage of Partial-upfront 1-Year per month (with Optimization)
#     instsPM3.opt: 
#       Instance usage of Partial-upfront 3-Year per month (with Optimization)
#     instsAM1.opt: 
#       Instance usage of All-upfront 1-Year per month (with Optimization)
#     instsAM3.opt: 
#       Instance usage of All-upfront 3-Year per month (with Optimization)
#     cost.base: 
#       Cost of each pricing policies per month (w/o Optimization)
#     cost.opt: 
#       Cost of each pricing policies per month (with Optimization)
performOptimization <- function(insts, rawUps, valueX, prices,
                                maxNU1, maxPU1, maxPU3, maxAU1, maxAU3,
                                printProg) {
  # Variables setting
  thisMonth <- insts[1, "Month"]
  optResult <- initOptResult(insts, valueX)
  # Check price information
  for (inst in names(optResult[["instsTotal"]])[-1]) {
    selectedPrices <- sum(prices$Instance == inst)
    if (selectedPrices != 1) {
      return(NULL)
    }
  }
  # Get upfronts
  if (printProg) {
    optResult <- getOptUpsWithProg(optResult, inst, rawUps, valueX, prices,
                                   maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                                   thisMonth)
  } else {
    optResult <- getOptUpsWithoutProg(optResult, inst, rawUps, valueX, prices,
                                      maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                                      thisMonth)
  }

  # Get instance usage per month
  optResult <- getInstanceUsage(optResult, rawUps, valueX, thisMonth)

  # Get cost
  optResult <- getCost(optResult, prices)
  # Get updated upfronts
  optResult <- updateUpfronts(optResult, rawUps)
  
  optResult
}

# Set initial optimal results
#
# Args:
#   insts: Instance usage
#   valueX: Simulation period
#
# Returns
#   Initial optimal results
initOptResult <- function(insts, valueX) {
  optResult <- list()
  
  optResult[["instsTotal"]] <- insts[1:min(valueX, nrow(insts)), ]
  if (valueX > nrow(insts)) {
    lastMonth <- insts[nrow(insts), 1]
    for (month.i in 1:(valueX - nrow(insts))) {
      newInsts <- 
        data.frame(
          Month = newYearMonth(lastMonth, month.i), 
          matrix(0, nrow = 1, ncol = ncol(insts) - 1)
        )
      names(newInsts) <- names(insts)
      optResult[["instsTotal"]] <- rbind(optResult[["instsTotal"]], newInsts)
    }
  }
  
  optResult[["instsODM.base"]] <- optResult[["instsTotal"]]
  optResult[["instsNM1.base"]] <- optResult[["instsTotal"]]
  optResult[["instsPM1.base"]] <- optResult[["instsTotal"]]
  optResult[["instsPM3.base"]] <- optResult[["instsTotal"]]
  optResult[["instsAM1.base"]] <- optResult[["instsTotal"]]
  optResult[["instsAM3.base"]] <- optResult[["instsTotal"]]
  optResult[["instsODM.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsNU1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPU1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPU3.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAU1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAU3.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsNM1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPM1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPM3.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAM1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAM3.opt"]] <- optResult[["instsTotal"]]
  optResult[["cost.base"]] <- optResult[["instsTotal"]]
  optResult[["cost.opt"]] <- optResult[["instsTotal"]]
  
  optResult[["instsODM.base"]][, -1] <- 0
  optResult[["instsNM1.base"]][, -1] <- 0
  optResult[["instsPM1.base"]][, -1] <- 0
  optResult[["instsPM3.base"]][, -1] <- 0
  optResult[["instsAM1.base"]][, -1] <- 0
  optResult[["instsAM3.base"]][, -1] <- 0
  optResult[["instsODM.opt"]][, -1] <- 0
  optResult[["instsNU1.opt"]][, -1] <- 0
  optResult[["instsPU1.opt"]][, -1] <- 0
  optResult[["instsPU3.opt"]][, -1] <- 0
  optResult[["instsAU1.opt"]][, -1] <- 0
  optResult[["instsAU3.opt"]][, -1] <- 0
  optResult[["instsNM1.opt"]][, -1] <- 0
  optResult[["instsPM1.opt"]][, -1] <- 0
  optResult[["instsPM3.opt"]][, -1] <- 0
  optResult[["instsAM1.opt"]][, -1] <- 0
  optResult[["instsAM3.opt"]][, -1] <- 0
  optResult[["cost.base"]][, -1] <- 0
  optResult[["cost.opt"]][, -1] <- 0
  
  optResult
}

# Get objective function of LP problem
#
# Args:
#   inst: Instance usage
#   valueX: Simulation period
#   prices: EC2 price information
#
# Returns:
#   Objective function
getObjFunc <- function(inst, valueX, prices) {
  c(
    rep(
      prices[prices$Instance == inst, "On.Demand_Month"], 
      valueX
    ),
    prices[prices$Instance == inst, "Part.UP.1Y_UP"] + 
      prices[prices$Instance == inst, "Part.UP.1Y_Month"] * 
      pmin(8760, valueX - seq(1:valueX) + 1)
  )
}

# Get optimized upfront with progress
#
# Args:
#   optResult: Optimal results
#   inst: target instance
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: maximum usage of percentage of all-upfront 3-year instances
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getOptUpsWithProg <- function(optResult, inst, rawUps, valueX, prices,
                              maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                              thisMonth) {
  withProgress(message = "Optimizing", value = 0, {
    for (inst in names(optResult[["instsTotal"]])[-1]) {
      optResult <- getOptUps(optResult, inst, rawUps, valueX, prices,
                             maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, thisMonth)
      incProgress(1 / (ncol(optResult[["instsTotal"]]) - 1), detail = inst)
    }
  })
  
  optResult
}

# Get optimized upfront without progress
#
# Args:
#   optResult: Optimal results
#   inst: target instance
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: maximum usage of percentage of all-upfront 3-year instances
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getOptUpsWithoutProg <- function(optResult, inst, rawUps, valueX, prices,
                                 maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                                 thisMonth) {
  for (inst in names(optResult[["instsTotal"]])[-1]) {
    optResult <- getOptUps(optResult, inst, rawUps, valueX, prices,
                           maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, thisMonth)
  }
  
  optResult
}

# Get optimized upfront instances
#
# Args:
#   optResult: Optimal results
#   inst: target instance
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: maximum usage of percentage of all-upfront 3-year instances
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getOptUps <- function(optResult, inst, rawUps, valueX, prices,
                      maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                      thisMonth) {
  print("Creating constraints...")
  lpResult <- make.lp(0, 2 * valueX)
  set.objfn(lpResult, getObjFunc(inst, valueX, prices))
  for (st.i in 1:valueX) {
    idx1y = max(1, st.i - 8759)
    xt <- c(numeric(st.i - 1), 
            1, 
            numeric(valueX - st.i))  # On-demand
    xt <- c(xt, 
            numeric(idx1y - 1), 
            rep(1, st.i - idx1y + 1), 
            numeric(valueX - st.i))  # Partial-upfront 1-Year
    rhs <- optResult[["instsTotal"]][st.i, inst]
    add.constraint(lpResult, xt, ">=", rhs)
    if(st.i %% 1000){
      print((st.i / valueX) *100)
    }
  }
  print("Creating constraints done.")
  print("Adding constraints...")
  set.bounds(lpResult, lower = numeric(2 * valueX))
  print("Adding constraints done")
  print("Solving...")
  solve(lpResult)
  print("Solved")
  optResult[["instsPU1.opt"]][, inst] <- 
    floor(get.variables(lpResult)[(1 * valueX + 1):(2 * valueX)])
  rm(lpResult)
  
  optResult
}

# Get instance usage per month
#
# Args:
#   optResult: Optimal results
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getInstanceUsage = function(optResult, rawUps, valueX, thisMonth) {
  for (month.i in 1:valueX) {
    if (!(is.null(rawUps)) & (month.i <= 8759)) {
      for (pre.i in 1:(8760 - month.i)) {
        preMonth <- newYearMonth(thisMonth, -pre.i)
        optResult[["instsNM1.base"]][month.i, -1] <- 
          optResult[["instsNM1.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsPM1.base"]][month.i, -1] <- 
          optResult[["instsPM1.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsAM1.base"]][month.i, -1] <- 
          optResult[["instsAM1.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsPM3.base"]][month.i, -1] <- 
          optResult[["instsPM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsAM3.base"]][month.i, -1] <- 
          optResult[["instsAM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, -1]
          )[-1]
      }
    }
    if (!(is.null(rawUps)) & (month.i <= 26279)) {
      for (pre.i in max(1, 8761 - month.i):(26280 - month.i)) {
        preMonth <- newYearMonth(thisMonth, -pre.i)
        optResult[["instsPM3.base"]][month.i, -1] <- 
          optResult[["instsPM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsAM3.base"]][month.i, -1] <- 
          optResult[["instsAM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, -1]
          )[-1]
      } 
    }
    optResult[["instsNM1.opt"]][month.i, -1] <- 
      optResult[["instsNM1.base"]][month.i, -1] + 
      colSums(optResult[["instsNU1.opt"]][max(1, month.i - 8759):month.i, ])[-1]
    optResult[["instsPM1.opt"]][month.i, -1] <- 
      optResult[["instsPM1.base"]][month.i, -1] + 
      colSums(optResult[["instsPU1.opt"]][max(1, month.i - 8759):month.i, ])[-1]
    optResult[["instsPM3.opt"]][month.i, -1] <- 
      optResult[["instsPM3.base"]][month.i, -1] + 
      colSums(optResult[["instsPU3.opt"]][max(1, month.i - 26281):month.i, ])[-1]
    optResult[["instsAM1.opt"]][month.i, -1] <- 
      optResult[["instsAM1.base"]][month.i, -1] + 
      colSums(optResult[["instsAU1.opt"]][max(1, month.i - 8759):month.i, ])[-1]
    optResult[["instsAM3.opt"]][month.i, -1] <- 
      optResult[["instsAM3.base"]][month.i, -1] + 
      colSums(optResult[["instsAU3.opt"]][max(1, month.i - 26281):month.i, ])[-1]
  }
  optResult[["instsODM.base"]][, -1] <- 
    pmax(0, as.matrix(optResult[["instsTotal"]][, -1] - 
                        optResult[["instsNM1.base"]][, -1] - 
                        optResult[["instsPM1.base"]][, -1] - 
                        optResult[["instsPM3.base"]][, -1] - 
                        optResult[["instsAM1.base"]][, -1] - 
                        optResult[["instsAM3.base"]][, -1]))
  optResult[["instsODM.opt"]][, -1] <- 
    pmax(0, as.matrix(optResult[["instsTotal"]][, -1] - 
                        optResult[["instsNM1.opt"]][, -1] - 
                        optResult[["instsPM1.opt"]][, -1] - 
                        optResult[["instsPM3.opt"]][, -1] - 
                        optResult[["instsAM1.opt"]][, -1] - 
                        optResult[["instsAM3.opt"]][, -1]))
  
  optResult
}

# Get cost
#
# Args:
#   optResult: Optimal results
#   prices: EC2 price information
#
# Returns:
#   Optimal results
getCost <- function(optResult, prices) {
  for (inst in names(optResult[["instsTotal"]])[-1]) {
    optResult[["cost.base"]][, inst] <- 
      optResult[["instsODM.base"]][, inst] * 
      prices[prices$Instance == inst, "On.Demand_Month"] +
      optResult[["instsNM1.base"]][, inst] * 
      prices[prices$Instance == inst, "No.UP.1Y_Month"] +
      optResult[["instsPM1.base"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.1Y_Month"] +
      optResult[["instsPM3.base"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.3Y_Month"]
    optResult[["cost.opt"]][, inst] <- 
      optResult[["instsODM.opt"]][, inst] * 
      prices[prices$Instance == inst, "On.Demand_Month"] +
      optResult[["instsPU1.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.1Y_UP"] +
      optResult[["instsPU3.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.3Y_UP"] +
      optResult[["instsAU1.opt"]][, inst] * 
      prices[prices$Instance == inst, "All.UP.1Y_UP"] +
      optResult[["instsAU3.opt"]][, inst] * 
      prices[prices$Instance == inst, "All.UP.3Y_UP"] +
      optResult[["instsNM1.opt"]][, inst] * 
      prices[prices$Instance == inst, "No.UP.1Y_Month"] +
      optResult[["instsPM1.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.1Y_Month"] +
      optResult[["instsPM3.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.3Y_Month"]
  }
  
  optResult
}

# Update upfronts
#
# Args:
#   optResult: Optimal results
#   rawUps: Previous upfront instances
#
# Returns:
#   Optimal results
updateUpfronts <- function(optResult, rawUps) {
  thisUps <- 
    rbind(data.frame(Pricing = "NU1", optResult[["instsNU1.opt"]][1, ]),
          data.frame(Pricing = "PU1", optResult[["instsPU1.opt"]][1, ]),
          data.frame(Pricing = "PU3", optResult[["instsPU3.opt"]][1, ]),
          data.frame(Pricing = "AU1", optResult[["instsAU1.opt"]][1, ]),
          data.frame(Pricing = "AU3", optResult[["instsAU3.opt"]][1, ]))
  if (!is.null(rawUps)) {
    optResult[["updateUps"]] <- rbind(rawUps, thisUps)
  }
  else {
    optResult[["updateUps"]] <- thisUps
  }
  
  optResult
}


