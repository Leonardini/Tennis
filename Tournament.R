library(tidyverse)
library(magrittr)
library(ape)
library(phytools)
library(lubridate)
library(googlesheets4)
gs4_deauth()
options(warn = 0)
pdf.options(title = paste("Draw as of", format(Sys.Date(), "%a %b %d")))

setwd("~/Downloads/Tennis/")
mySeed = 13251259L
startDate = as.Date("2021-06-11")
numberOfDays = 84L
POWERS = 2^(0:6)
Singleton = list(Nnode = 0, tip.label = "1", edge = matrix(NA, nrow = 0, ncol = 2))
class(Singleton) = "phylo"
Cherry = list(Nnode = 1, tip.label = c("1", "2"), edge = rbind(c(3,1), c(3,2)), edge.length = rep(1, 2))
class(Cherry) = "phylo"

### This function computes the indices to use for maximizing tree distances between pairs of highly seeded players
computeIndices = function(numRounds = 5) {
  stopifnot(numRounds > 0)
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

getParents = function(tree, nodeNumbers) {
  parents = tree$edge[getParentEdges(tree, nodeNumbers), 1]
  parents
}

getParentEdges = function(tree, nodeNumbers) {
  parentEdges = match(nodeNumbers, tree$edge[,2])
  parentEdges
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

createFullDirectory = function() {
  TabDir = read_csv("Directory.csv")
  doublesDir = "SeededDoubles/"
  LF2 = list.files(path = doublesDir, pattern = ".csv")
  fullDir = TabDir
  fullNames = paste0(TabDir$Forename, TabDir$Surname)
  for (fname in LF2) {
    print(fname)
    extraTab = read_csv(paste0(doublesDir, fname))
    extraParticipants = extraTab %>%
      select(Forename, Surname)
    extraPartners = extraTab %>%
      select(PartnerForename, PartnerSurname) %>%
      rename(Forename = PartnerForename, Surname = PartnerSurname)
    extraParticipants = extraParticipants %>%
      bind_rows(extraPartners) %>%
      mutate(fullName = paste0(Forename, Surname), listed = map_lgl(fullName, ~{any(agrepl(., fullNames))})) %>%
      filter(!listed) %>%
      select(-fullName, -listed)
    fullDir = fullDir %>%
      bind_rows(extraParticipants)
  }
  fullDir = fullDir %>%
    distinct(Forename, Surname, .keep_all = TRUE) %>%
    arrange(Surname)
  fullDir = fullDir %>%
    mutate_at("Phone", ~{
      as.character(.) %>%
      str_remove("^44") %>%
      str_replace("^7", "07") %>%
      str_replace("NA", "") %>%
      paste0("\t", .)}) %>%
    mutate_at("Email", ~{as.character(.) %>% 
        str_replace("NA", "")})
  write_csv(fullDir, "FullDirectory.csv")
  fullDir
}

createAllBrackets = function(singlesDir = "SeededSingles/", doublesDir = "SeededDoubles/") {
  if (!is.null(singlesDir)) {
    LF1 = list.files(path = singlesDir, pattern = ".csv")
    for (fname in LF1) {
      Z = createBracket(paste0(singlesDir, fname), singles = TRUE, mark = TRUE)
    }
  }
  if (!is.null(doublesDir)) {
    LF2 = list.files(path = doublesDir, pattern = ".csv")
    for (fname in LF2) {
      W = createBracket(paste0(doublesDir, fname), singles = FALSE, mark = TRUE)
    }
  }
  return(TRUE)
}

processLatestResults = function() {
  Tab = read_sheet("https://docs.google.com/spreadsheets/d/1GNerCMbSkztdg4CSOSQNqjJWD3ozVCesEJ9wmi-Bumo/edit?resourcekey#gid=73994542") %>%
    select(Timestamp, `Which event would you like to enter a match result for?`, `Winner's name(s)`, `Loser's name(s)`, `What was the match score?`) %>%
    rename(Time = Timestamp, Event = `Which event would you like to enter a match result for?`, 
           Winner = `Winner's name(s)`, Loser = `Loser's name(s)`, Score = `What was the match score?`) %>%
    mutate_at("Time", ~{format(., "%d-%m-%Y")}) %>%
    mutate_all(~{str_trim(., side = "both")})
  fname = paste0("Results_", str_replace_all(Sys.Date(), " ", "_"), ".csv")
  write_csv(Tab, fname)
  Tab = Tab %>%
    mutate_at("Event", ~{str_replace_all(., "\\ ", "_")}) %>%
    mutate(Doubles = str_detect(Event, "doubles"))
  TabG = Tab %>%
    group_by(Event) %>%
    group_split()
  N = length(TabG)
  missing = c()
  for (index in 1:N) {
    curTab = TabG[[index]]
    curEvent = curTab$Event[1]
    print(curEvent)
    curDoubles = curTab$Doubles[1]
    curFile = paste0("SeededAll", "/Entries_", curEvent, "FullySeeded.csv")
    curDraw = read_csv(curFile)
    if (curDoubles) {
      curNames = curDraw %>%
        mutate(fullName = paste(Forename, Surname, "and", PartnerForename, PartnerSurname)) %>%
        pull(fullName)
    } else {
      curNames = curDraw %>% 
        mutate(fullName = paste(Forename, Surname)) %>%
        pull(fullName)
    }
    usedNames = curTab$Winner
    if (curDoubles) { ### Need to order each pair alphabetically!
      usedNames = usedNames %>% 
        str_split_fixed(" and ", n = 2) %>%
        apply(1, function(x) {paste(sort(x), collapse = " and ")})
      curTab$Winner = usedNames
    }
    if (!all(usedNames %in% curNames)) {
      print(paste("Warning:", paste(setdiff(usedNames, curNames), collapse = ", "), "have not been found in the draw!"))
      missing = c(missing, setdiff(usedNames, curNames))
      next
    }
    baseFname = str_remove(curFile, "SeededAll/") %>% 
        str_replace("FullySeeded.csv", ".csv")
    updateBracket(initDraw = curDraw, newResults = curTab, singles = !(curDoubles), baseFname = baseFname, mark = TRUE)
  }
  output = list(Tab, missing)
  output
}

updateBracket = function(initDraw, newResults = NULL, singles = FALSE, baseFname = NULL, mark = TRUE, TBD = FALSE) {
  BYE = ifelse(TBD, "TBD", "BYE")
  N = nrow(initDraw)
  fullN = min(POWERS[POWERS >= N])
  numByes = fullN - N
  if (any(is.na(initDraw$Seed))) {
    print(paste("Generating seeds randomly for the", sum(is.na(initDraw$Seed)), "currently unseeded players"))
    set.seed(mySeed)
    maxAssignedSeed = ifelse(any(!is.na(initDraw$Seed)), max(initDraw$Seed, na.rm = TRUE), 0L)
    stopifnot(sum(is.na(initDraw$Seed)) == N - maxAssignedSeed)
    restSeeds = sample((maxAssignedSeed + 1):N, replace = FALSE)
    initDraw$Seed[is.na(initDraw$Seed)] = restSeeds
  }
  if (numByes > 0) {
    extraDraw = tibble(Forename = BYE, Surname = "", Seed = N + (1:numByes))
    initDraw  = bind_rows(initDraw, extraDraw)
  }
  numByes = sum(initDraw$Forename == BYE)
  numRounds = as.integer(round(log2(fullN)))
  initTree  = mergeSubtrees(Singleton, Singleton)
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
  initDraw = initDraw %>%
    arrange(Seed) %>%
    slice(goodIndices) %>%
    mutate(fullName = paste(Forename, Surname)) %>%
    mutate(shortName = paste(Forename, paste0(str_sub(Surname, 1, 1), ".")))
  edgeLabs  = initDraw$fullName
  fullNames = edgeLabs
  if (!singles) {
    initDraw = initDraw %>%
      mutate(fullPartnerName = paste(PartnerForename, PartnerSurname)) %>%
      mutate(shortPartnerName = paste(PartnerForename, paste0(str_sub(PartnerSurname, 1, 1), "."))) %>%
      mutate(fullTeamName = paste(fullName, "and", fullPartnerName)) %>%
      mutate(shortTeamName = paste(shortName, "and", shortPartnerName))
    edgeLabs  = initDraw$shortTeamName
    fullNames = initDraw$fullTeamName
  }
  if (numByes > 0) {
    edgeLabs [which(initDraw$Forename == BYE)] = BYE
    fullNames[which(initDraw$Forename == BYE)] = BYE  
  }
  drawFname = baseFname %>% 
    str_replace(".csv", ".pdf")
  pdf(drawFname, height = fullN * 0.18 + 2, width = numRounds * 1.5 + 1, compress = FALSE)
  plotTree(initTree, direction = "leftwards", tips = tipInds, node.numbers = FALSE, offset = 10)
  if (fullN > 2 && mark) {
    pickNodes = match(c(1, 2), initDraw$Seed)
    nodelabels(c(1, 2), pickNodes, frame = "circle", bg = "white", col = "black", cex = 0.5)
  }
  pickEdges = getParentEdges(initTree, 1:fullN)
  edgelabels(edgeLabs, pickEdges, frame = "none", bg = "white", adj = c(0.55, -0.45), cex = 0.5)
  if (numByes > 0 && !TBD) {
    byeNodes = match(1:numByes, initDraw$Seed)
    promotedEdges = getParentEdges(initTree, getParents(initTree, byeNodes))
    edgelabels(edgeLabs[byeNodes], promotedEdges, frame = "none", bg = "white", adj = c(0.55, -0.45), cex = 0.5)
  }
  if (!is.null(newResults)) {
    curDraw = initDraw %>%
      mutate(Height = 1 + as.integer(Seed <= numByes))
    for (ind in 1:nrow(newResults)) {
      curResult =  newResults %>%
        slice(ind)
      curWinner = curResult$Winner
      curScore  = curResult$Score %>% 
        str_split(",") %>%
        unlist() %>%
        str_trim(side = "both")
      if (curScore[1] == "Walkover") { ### special case!
        curScore = "Walk \nover "
      } else {
        curScore = paste0(paste(curScore, collapse = " \n"), " ")
      }
      curNode   = match(curWinner, fullNames)
      shortWinner = edgeLabs[curNode]
      curHeight = curDraw %>%
        slice(curNode) %>%
        pull(Height)
      curDraw$Height[curNode] %<>%
        add(1)
      for (h in 1:curHeight) {
        curNode   = getParents(initTree, curNode)
      }
      nodelabels(curScore, curNode, frame = "rect", col = "red", bg = "white", cex = 0.5)
      curEdge = getParentEdges(initTree, curNode)
      edgelabels(shortWinner, curEdge, frame = "none", bg = "white", adj = c(0.55, -0.45), cex = 0.5)
    }
  }
  dev.off()
  initDraw
}

createBracket = function(fname = "SeededSingles/Entries_Men's_singles.csv", singles = TRUE, mark = TRUE, TBD = FALSE) {
  Tab = read_csv(fname) %>%
    mutate_at("Seed", as.integer)
  baseFname = str_remove(fname, "Seeded(Singles|Doubles|All)/")
  Tab = updateBracket(initDraw = Tab, newResults = NULL, singles = singles, baseFname = baseFname, mark = mark, TBD = TBD)
  seedFname = baseFname %>%
    paste0("SeededAll/", .)
  if (!str_detect(seedFname, "FullySeeded.csv")) {
    str_replace(seedFname, ".csv", "FullySeeded.csv")
  }
  if (!file.exists(seedFname)) { 
    write_csv(Tab, seedFname) 
  }
  Tab
}

reorderDoublesDraw = function(fname = "SeededAll/Entries_Mixed_doublesFullySeeded.csv") {
  Tab = read_csv(fname)
  TabM = Tab %>%
    rowid_to_column("index") %>%
    mutate(reorder = (Forename > PartnerForename) | (Forename == PartnerForename & Surname > PartnerSurname))
  TabR = TabM %>%
    filter(reorder) %>%
    mutate(tempForename = Forename, Forename =  PartnerForename, PartnerForename = tempForename) %>%
    mutate(tempSurname  =  Surname,  Surname =   PartnerSurname, PartnerSurname  =  tempSurname) %>%
    select(-tempForename, -tempSurname)
  TabO = TabM %>%
    filter(is.na(reorder) | !reorder)
  TabM = bind_rows(TabO, TabR) %>%
    arrange(index) %>%
    select(-index)
  write_csv(TabM, fname)
  TabM
}

reorderDoublesDraws = function() {
  initDir = getwd()
  setwd("SeededAll/")
  LF = list.files(pattern = "doubles")
  for (fname in LF) {
    print(fname)
    reorderDoublesDraw(fname)
  }
  setwd(initDir)
}

makePlateDraws = function(event = "Men's", singles = TRUE, removeSpecial = ifelse(event == "Men's" && singles, 10, 0), 
                          TBD = FALSE, firstMatch = FALSE) {
  BYE = ifelse(TBD, "TBD", "BYE")
  ext = ifelse(singles, "singles", "doubles")
  latestResults = read_csv(paste0("Results_", str_replace_all(Sys.Date(), " ", "_"), ".csv"))
  initDraw = read_csv(paste0("SeededAll/Entries_", event, "_", ext, "FullySeeded.csv"))
  N = nrow(initDraw)
  initDraw = initDraw %>%
    mutate(index = rep(1:(N/2), each = 2)) %>%
    mutate(bye = (Forename == "BYE"))
  if (firstMatch) {
    skipIndices = 0
    initDraw = initDraw %>%
      mutate(index = rep(1:(N/4), each = 4)) %>%
      filter(!bye)
  } else {
    skipIndices = initDraw %>%
      filter(bye == TRUE) %>%
      pull(index)
  }
  initDraw = initDraw %>%
    filter(!(index %in% skipIndices)) %>%
    mutate(fullName = paste(Forename, Surname))
  relevantResults = latestResults %>%
    filter(Event == paste(event, ext))
  relevantWinners = relevantResults %>%
    pull(Winner)
  relevantWalkovers = relevantResults %>%
    filter(Score == "Walkover") %>%
    pull(Winner)
  if (!singles) {
    relevantWinners = relevantWinners %>%
      str_split_fixed(" and ", n = 2) %>%
      as.vector() %>%
      str_trim()
  }
  if (length(relevantWalkovers) > 0) {
    if (!singles) {
      relevantWalkovers = relevantWalkovers %>%
        str_split_fixed(" and ", n = 2) %>%
        as.vector() %>%
        str_trim()
    }
    walkoverIndices = initDraw %>%
      filter(fullName %in% relevantWalkovers) %>%
      pull(index)
    initDraw = initDraw %>%
      filter(!(index %in% walkoverIndices))
  }
  initDraw = initDraw %>%
    mutate(winner = (fullName %in% relevantWinners)) %>%
    filter(!winner)
  if (removeSpecial != 0) {
    initDraw = initDraw %>%
      slice(-removeSpecial)
  }
  if (TBD) {
    initDraw = initDraw %>%
      group_by(index) %>%
      mutate(N = n()) %>%
      ungroup()
    numExtras = N/ifelse(firstMatch, 4, 2) - sum(initDraw$N == 1)
    initDraw = initDraw %>%
      filter(N == 1) %>%
      bind_rows(tibble(Forename = BYE, Surname = "", Seed = 1:numExtras))
  }
  initDraw = initDraw %>%
    select(-Seed, -index, -bye, -fullName, -winner)
  set.seed(seed = mySeed)
  M = nrow(initDraw)
  initDraw = initDraw %>%
    mutate(Seed = sample(1:M, replace = FALSE))
  fn = paste0("Seeded", str_to_title(ext), "/Entries_", event, "_", ext, "_plate.csv")
  write_csv(initDraw, fn)
  result = createBracket(fname = fn, singles = singles, mark = FALSE, TBD = TBD)
  result
}

### Daily process:
### Z = processLatestResults()
### If plate brackets are needed:
### Q = makePlateDraws()
####
