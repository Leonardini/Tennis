library(tidyverse)
library(magrittr)
library(ape)
library(phytools)
library(lubridate)
setwd("~/Downloads/Tennis/")
mySeed = 13251259L
startDate = as.Date("2021-06-11")
numberOfDays = 84L
options(warn = 0)

Singleton = list(Nnode = 0, tip.label = "1", edge = matrix(NA, nrow = 0, ncol = 2))
class(Singleton) = "phylo"
Cherry = list(Nnode = 1, tip.label = c("1", "2"), edge = rbind(c(3,1), c(3,2)), edge.length = rep(1, 2))
class(Cherry) = "phylo"

### This function merges two subtrees into one; it is similar to bind.tree with added root edges
mergeSubtrees = function(tree1, tree2) {
  L1 = tree1$Nnode + 1
  L2 = tree2$Nnode + 1
  L = L1 + L2
  tree = list(Nnode = L - 1, tip.label = 1:L)
  Edges1 = tree1$edge
  Edges2 = tree2$edge
  Edges1[Edges1 > L1]  = Edges1[Edges1 > L1] + L2 + 1
  Edges2[Edges2 > L2]  = Edges2[Edges2 > L2] + 2 * L1
  Edges2[Edges2 <= L2] = Edges2[Edges2 <= L2] + L1
  root = L + 1
  root1 = ifelse (L1 == 1, 1, root + 1)
  root2 = ifelse (L2 == 1, L, root + L1)
  tree$edge = rbind(c(root, root1), c(root, root2), Edges1, Edges2)
  class(tree) = "phylo"
  tree
}

prepareInitialSpreasheets = function() {
  Tab = read_csv("TournamentRegistrationEdited.csv")
  Tab = Tab %>%
    select(-9) %>%
    select(-Email) %>%
    rename("Time" = "Timestamp", "Name" = `Player's name`, "Partner" = `If playing doubles, your partner's full name`,
           "Gender" = `Gender (This answer will only be used to confirm your eligibility for the chosen section)`,
           "Birthday" = `Date of birth (This answer will only be used to confirm your eligibility for the chosen section)`,
           "Singles" = `Which singles events would you like to enter?`, "Doubles" = `Which doubles events would you like to enter?`,
           "Phone" = `Contact phone number`, "Email" = `Contact email address`) %>%
    select(-Gender, -Birthday, -Time)
  TabR = Tab %>%
    filter(!is.na(Name)) %>%
    filter(!str_sub(Name,1,4)=="TEST" & !str_sub(Name,-4,-1)=="TEST")
  TabR = TabR %>%
    filter(Name != "Roger federer")
  TabR = TabR %>%
    mutate_at("Name", ~{str_replace_all(., "é", "e") %>% str_remove_all("\\([a-zA-Z\\ ]*\\)")})
  TabR = TabR %>%
    separate("Name", c("Forename", "Surname"), sep = "[\\ ]+", extra = "merge")
  TabR = TabR %>%
    mutate_at(c("Forename", "Surname"), ~{str_to_title(.) %>% str_remove_all(" ")})
  TabR = TabR %>%
    mutate_at("Phone", ~{str_replace(., "\\+44", "0")})
  TabDir = TabR %>%
    select(c(Forename, Surname, Phone, Email))
  TabDir = TabDir %>%
    mutate_at("Email", ~{tolower(.)}) %>%
    group_by(Forename, Surname) %>%
    slice(1) %>%
    ungroup()
  write_csv(TabDir, path = "Directory.csv")
  TabS = TabR %>%
    filter(!is.na(Singles)) %>%
    select(-Doubles, -Phone, -Partner)
  TabS = TabS %>%
    mutate_at("Singles", ~{str_split(., ", ")}) %>%
    unnest(Singles) %>%
    mutate_at("Singles", ~{str_remove_all(., "\\([a-zA-Z0-9\\ ]*\\)")}) %>%
    mutate_at("Singles", str_trim) %>%
    group_by(Forename, Surname, Singles) %>%
    slice(1) %>%
    ungroup() %>%
    rename("Event" = "Singles")
  uEvents = sort(unique(TabS$Event))
  for (event in uEvents) {
    redTab = TabS %>%
      filter(Event == event) %>%
      select(-Event) %>%
      mutate(Seed = " ")
    write_csv(redTab, paste0("Entries_", str_replace_all(event, "\\ ", "\\_"), ".csv"))
  }
  TabD = TabR %>%
    filter(!is.na(Doubles)) %>%
    select(-Singles, -Phone)
  TabD = TabD %>%
    filter(!is.na(Partner)) %>%
    mutate_at("Doubles", ~{str_split(., ", ")}) %>%
    unnest(Doubles) %>%
    mutate_at("Doubles", ~{str_remove_all(., "\\([a-zA-Z0-9\\ ]*\\)")}) %>%
    mutate_at("Doubles", str_trim)
  allNames = TabDir %>%
    mutate(FullName = paste(Forename, Surname)) %>%
    pull(FullName) %>%
    sort() %>%
    tolower()
  TabD = TabD %>%
    mutate(Partner = tolower(Partner)) %>%
    mutate_at("Partner", ~{str_replace_all(., "é", "e")}) %>%
    mutate_at("Partner", ~{map(., function(x) {str_split(x, "; ") %>% unlist()})}) %>%
    unnest(Partner)
  TabSep = TabD %>%
    separate("Partner", into = c("Event", "Partner"), sep = ": ", fill = "left") %>%
    mutate_at("Event", ~{str_replace_all(., "\\’", "\\'")} %>% str_trim() %>% str_replace_all("over 50", "50 and over"))
  problemD = TabSep %>%
    filter(!(is.na(Event) | Event == tolower(Doubles)))
  TabSep = TabSep %>%
    filter(is.na(Event) | Event == tolower(Doubles))
  problemD = TabSep %>%
    filter(nchar(TabSep$Partner) > max(nchar(allNames)) + 1)
  TabSep = TabSep %>%
    filter(nchar(TabSep$Partner) <= max(nchar(allNames)) + 1) %>%
    select(-Event) %>%
    rename("Event" = "Doubles")
  TabSep = TabSep %>%
    separate(Partner, into = c("PartnerForename", "PartnerSurname"), sep = " ", fill = "right") %>%
    mutate_at(c("PartnerForename", "PartnerSurname"), str_to_title)
  TabOrd = TabSep %>%
    mutate(inOrder = (Forename < PartnerForename)) %>%
    mutate(fullName = paste(Forename, Surname)) %>%
    mutate(partnerName = paste(PartnerForename, PartnerSurname))
  TabP = TabOrd %>%
    filter(inOrder)
  TabN = TabOrd %>%
    filter(!inOrder) %>%
    mutate(temp = fullName, fullName = partnerName, partnerName = temp) %>%
    select(-temp)
  TabOrd = bind_rows(TabP, TabN) %>%
    mutate(team = paste(fullName, "and", partnerName)) %>%
    group_by(team, Event) %>%
    slice(1) %>%
    ungroup()
  TabOrd = TabOrd %>%
    select(-inOrder, -team)
  uEvents = sort(unique(TabOrd$Event))
  for (event in uEvents) {
    redTab = TabOrd %>%
      filter(Event == event) %>%
      select(-Event) %>%
      mutate(Seed = " ")
    curNames = c(redTab$fullName, redTab$partnerName)
    if (length(unique(curNames)) != length(curNames)) {
      print(paste("Discrepancies found:", event))
      print(curNames[duplicated(curNames)])
    }
    redTab = redTab %>%
      select(-fullName, -partnerName)
    write_csv(redTab, paste0("Entries_", str_replace_all(event, "\\ ", "\\_"), ".csv"))
  }
  problemD = problemD %>%
    select(-Event)
  write_csv(problemD, "ProblematicEntries.csv")
}

createAllBrackets = function() {
  singlesDir = "SeededSingles/"
  LF1 = list.files(path = singlesDir, pattern = ".csv")
  for (fname in LF1) {
    print(fname)
    Z = createBracket(paste0(singlesDir, fname), singles = TRUE)
  }
  doublesDir = "SeededDoubles/"
  LF2 = list.files(path = doublesDir, pattern = ".csv")
  for (fname in LF2) {
    print(fname)
    W = createBracket(paste0(doublesDir, fname), singles = FALSE)
  }
  return(TRUE)
}

createBracket = function(fname = "SeededSingles/Entries_Men's_singles.csv", singles = TRUE) {
  set.seed(mySeed)
  Tab = read_csv(fname) %>%
    mutate_at("Seed", as.integer)
  N = nrow(Tab)
  powers = 2^(0:6)
  fullN = min(powers[powers >= N])
  numByes = fullN - N
  maxAssignedSeed = max(Tab$Seed, na.rm = TRUE)
  if (!is.finite(maxAssignedSeed)) {
    maxAssignedSeed = 0L
  }
  stopifnot(sum(is.na(Tab$Seed)) == N - maxAssignedSeed)
  if (any(is.na(Tab$Seed))) {
    restSeeds = sample((maxAssignedSeed + 1):N, replace = FALSE)
    Tab$Seed[is.na(Tab$Seed)] = restSeeds
  }
  if (numByes > 0) {
    extraTab = tibble(Forename = "BYE", Surname = "", Seed = N + (1:numByes))
    Tab = bind_rows(Tab, extraTab)
  }
  numRounds = as.integer(round(log2(fullN)))
  initTree = mergeSubtrees(Singleton, Singleton)
  if (numRounds > 1) {
    for (ind in 1:(numRounds - 1)) {
      initTree = mergeSubtrees(initTree, initTree)
    }
  }
  initTree$root.edge <- 1
  initTree$edge.length = rep(1, nrow(initTree$edge))
  attr(initTree,"order") = "cladewise"
  initTree = rootedge.to.singleton(initTree)
  if (Ntip(initTree) > 2) {
    tipInds = setNames(round(1.5 * (initTree$tip.label - 0.5)), initTree$tip.label)
  } else {
    tipInds = setNames(initTree$tip.label, initTree$tip.label)
  }
  goodIndices = computeIndices(numRounds = numRounds)
  Tab = Tab %>%
    arrange(Seed) %>%
    slice(goodIndices)
  if (singles) {
    edgeLabs = paste(Tab$Forename, Tab$Surname)
  } else {
    edgeLabs = paste(Tab$Forename, "and", Tab$PartnerForename)
    if (numByes > 0) {
      edgeLabs[which(Tab$Forename == "BYE")] = "BYE"
    }
  }
  endDates = startDate + numberOfDays/(numRounds - 1)*(1:(numRounds - 1))
  # jpeg(str_replace(fname, ".csv", ".jpg"), height = fullN * 20, width = numRounds * 150, type = "quartz")
  # png(str_replace(fname, ".csv", ".png"), height = fullN * 20, width = numRounds * 150, type = "quartz")
  pdf(str_replace(fname, ".csv", ".pdf"), height = fullN * 0.18 + 2, width = numRounds * 1.5 + 1, compress = FALSE)
  plotTree(initTree, direction = "leftwards", tips = tipInds, node.numbers = FALSE, offset = 10)
  pickNodes = c(which(Tab$Seed == 1), which(Tab$Seed == 2))
  nodelabels(c(1, 2), pickNodes, frame = "circle", bg = "white", col = "black", cex = 0.5)
  lastPP = get("last_plot.phylo", envir = .PlotPhyloEnv)
  pickEdges = match(1:fullN, lastPP$edge[,2])
  edgelabels(edgeLabs, pickEdges, frame = "none", bg = "white", adj = c(0.6, -0.45), cex = 0.5)
  # axisPhylo(side = 1, root.time = 1, backward = TRUE)
  # pickNode = lastPP$edge[lastPP$edge[,2] == 17, 1]
  # nodelabels("0-6\n0-6", pickNode, frame = "rect", col = "red", bg = "white", cex = 0.5)
  dev.off()
  write_csv(Tab, str_replace(fname, ".csv", "FullySeeded.csv"))
}

computeIndices = function(numRounds = 5) {
  numRows = 2^numRounds
  Mat = matrix(NA, numRows, numRounds)
  init = c(TRUE, FALSE)
  for (ind in 1:numRounds) {
    Mat[, ind] = init
    init = c(init, !init)
  }
  Inds = Mat %*% 2^((numRounds - 1):0) + 1
  Inds
}
