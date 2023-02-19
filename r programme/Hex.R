# using tidyverse library
library(tidyverse)


#############################################################################

# FIRST STEP IS TO SET UP DATA SETS FOR TILE SETS AND OPTIONS
# FOR ALL POSSIBLE ARRANGEMENTS OF TILES

# set up an array of all potential position combinations of 7 hexagonal tiles.
# tile positions are numbered 0 to 6, 0 is centre position
# others are numbered 1 to 6 clockwise starting at 12 o'clock
start_time <- Sys.time() # to time process
combos <- data.frame()
testArray2 <- data.frame()
s1 <- c(1, 2, 3, 4, 5, 6, 7)
for (i1 in 1:7) {
  s2 <- s1[-i1]
  for (i2 in 1:6) {
    s3 <- s2[-i2]
    for (i3 in 1:5) {
      s4 <- s3[-i3]
      for (i4 in 1:4) {
        s5 <- s4[-i4]
        for (i5 in 1:3) {
          s6 <- s5[-i5]
          for (i6 in 1:2) {
            s7 <- s6[-i6]
            testArray2 <-
              c(s1[i1], s2[i2], s3[i3], s4[i4], s5[i5], s6[i6], s7[1])
            combos <- rbind(combos, testArray2)
          }
        }
      }
    }
  }
}
end_time <- Sys.time()
end_time - start_time  # Time difference of 1.437077 secs (first attempt took 14 hours!)

# save file of potential combinations to save re-running this
write.csv(combos, "combinations.csv", row.names = FALSE)

# read file of possible combinations
combos <- read.csv("combinations.csv")

# hexagonal tiles have each of six edges marked with colour, number or image
# programmed as numbers 1 to 6, each as an array starting with position number 1
# and sequence going clockwise

# read file of hexagonal tiles from csv
tiles <- read.csv("tiles.csv")

# or use a predefined set

# a set of tiles with a single solution
tiles <- tibble(
  t1 = 1,
  t2 = 2,
  t3 = 3,
  t4 = 4,
  t5 = 5,
  t6 = 6
)
tiles <- rbind(
  tiles,
  c(1, 5, 3, 6, 2, 4),
  c(1, 4, 3, 5, 2, 6),
  c(1, 2, 3, 4, 6, 5),
  c(1, 2, 5, 6, 3, 4),
  c(1, 6, 2, 5, 3, 4),
  c(1, 3, 6, 4, 5, 2)
)

# a set of tiles with no solutions
tiles <- tibble(
  t1 = 1,
  t2 = 2,
  t3 = 3,
  t4 = 4,
  t5 = 5,
  t6 = 6
)
tiles <- rbind(
  tiles,
  c(1, 5, 3, 6, 2, 4),
  c(1, 6, 5, 2, 3, 4),
  c(1, 2, 3, 4, 6, 5),
  c(1, 5, 2, 3, 6, 4),
  c(1, 6, 2, 5, 4, 3),
  c(1, 3, 6, 4, 2, 5)
)

# a set of tiles with multiple (4) solutions
tiles <- tibble(
  t1 = 1,
  t2 = 2,
  t3 = 3,
  t4 = 4,
  t5 = 5,
  t6 = 6
)
tiles <- rbind(
  tiles,
  c(1, 5, 4, 3, 2, 6),
  c(1, 2, 3, 4, 5, 6),
  c(1, 5, 2, 6, 3, 4),
  c(1, 3, 5, 4, 6, 2),
  c(1, 6, 2, 5, 3, 4),
  c(1, 5, 3, 6, 2, 4)
)

# and one with 8 solutions
tiles <- tibble(
  t1 = 1,
  t2 = 2,
  t3 = 3,
  t4 = 4,
  t5 = 5,
  t6 = 6
)
tiles <- rbind(
  tiles,
  c(1, 5, 2, 6, 3, 4),
  c(1, 2, 3, 4, 5, 6),
  c(1, 3, 5, 6, 2, 4),
  c(1, 5, 2, 6, 3, 4),
  c(1, 5, 3, 2, 6, 4),
  c(1, 3, 5, 4, 2, 6)
)

# a set of tiles with multiple (4) solutions
tiles <- tibble(
  t1 = 1,
  t2 = 2,
  t3 = 3,
  t4 = 4,
  t5 = 5,
  t6 = 6
)
tiles <- rbind(
  tiles,
  c(1, 3, 6, 4, 5, 2),
  c(1, 2, 3, 4, 6, 5),
  c(1, 4, 3, 5, 2, 6),
  c(1, 2, 5, 6, 3, 4),
  c(1, 6, 2, 5, 3, 4),
  c(1, 5, 3, 6, 2, 4)
)

######################################################################################
# DEFINE FUNCTIONS TO GET POSITIONS OF TILES AROUND CENTRE TILE
# AND CHECK WHETHER ALL POSITIONS MATCH

# function to take two tiles - tile1 is central tile, tile2 is test tile
# and the position number of the tile is counted around the central tile (counting clockwise)
# the function returns a number for the orientation to match with central tile
# i.e. this is the number of clockwise rotation steps to match the with centre tile
getOrientation <- function(tile1, tile2, position) {
  for (i in 1:6) {
    if (tile1[position] == tile2[i]) {
      return(i)
    }
  }
}

# this function gets the orientation for a set of all seven tiles
# and checks whether there is one or more valid solutions
checkCombo <- function(combi, tileset) {
  ori <-
    c(0, 0, 0, 0, 0, 0) # a vector to hold orientation for each outside tile
  orip <-
    c(0, 0, 0, 0, 0, 0) # orientations for plus and minus triangles that need to match
  orim <-
    c(0, 0, 0, 0, 0, 0) # these are calculated separately to make more readable
  for (i in 2:7) {
    ori[i] <-
      getOrientation(tileset[combi[[1]],], tileset[combi[[i]],], i - 1)
    orip[i] <-
      ifelse(ori[i] == 6, 1, ori[i] + 1) # these need adjusting to account for >6 or <1
    orim[i] <-
      ifelse(ori[i] == 1, 6, ori[i] - 1) # r is one based so mod 6 doesn't work
  }
  #if all outside tiles match return True, otherwise false
  return(
    tileset[combi[[2]], orim[2]] == tileset[combi[[3]], orip[3]] &&
      tileset[combi[[3]], orim[3]] == tileset[combi[[4]], orip[4]] &&
      tileset[combi[[4]], orim[4]] == tileset[combi[[5]], orip[5]] &&
      tileset[combi[[5]], orim[5]] == tileset[combi[[6]], orip[6]] &&
      tileset[combi[[6]], orim[6]] == tileset[combi[[7]], orip[7]] &&
      tileset[combi[[7]], orim[7]] == tileset[combi[[2]], orip[2]]
  )
}

#########################################################################
# MAIN PROGRAMME TESTS ALL 5040 COMBINATIONS OF GIVEN SET OF TILES
# AND PRINTS THE NUMBER OF VALID ANSWERS AND LISTS VALID COMBINATIONS

# define test variable to hold tests
test <- T

# test all possible arrangement options in combos data
for (i in 1:5040) {
  test[i] <- checkCombo(combos[i,], tiles)
}

# check how many valid answers there are
sum(test)

# show the valid answers
combos[test,]


##########################################################
# add section to check a bunch of random tile combination

# create all possible hex tile combinations
hexTilesAll <- data.frame()
testArray2 <- data.frame()
s1 <- c(2, 3, 4, 5, 6)
for (i1 in 1:5) {
  s2 <- s1[-i1]
  for (i2 in 1:4) {
    s3 <- s2[-i2]
    for (i3 in 1:3) {
      s4 <- s3[-i3]
      for (i4 in 1:2) {
        s5 <- s4[-i4]
        testArray2 <- c(1, s1[i1], s2[i2], s3[i3], s4[i4], s5[1])
        hexTilesAll <- rbind(hexTilesAll, testArray2)
      }
    }
  }
}


start_time <- Sys.time() # to time process
for (j in 1:5000) {
  testGroup <- hexTilesAll[sample(1:nrow(hexTilesAll), 7), ]
  # test all possible arrangement options in combos data
  for (i in 1:5040) {
    test[i] <- checkCombo(combos[i,], testGroup)
  }
  if (j %% 100 == 0) {
    print(j)
    print(Sys.time() - start_time)
  }
  # check how many valid answers there are
  if (sum(test) < 0) {
    print(testGroup)
    break
  }
}
end_time <- Sys.time()
end_time - start_time  # Time difference of 1.437077 secs (first attempt took 14 hours!)

tiles <- tibble(
  t1 = 1,
  t2 = 2,
  t3 = 3,
  t4 = 4,
  t5 = 5,
  t6 = 6
)
tiles <- rbind(
  tiles,
  c(1, 3, 2, 6, 5, 4),
  c(1, 6, 3, 5, 2, 4),
  c(1, 6, 3, 5, 2, 4),
  c(1, 4, 6, 5, 3, 2),
  c(1, 4, 3, 6, 2, 5),
  c(1, 5, 3, 6, 2, 4)
)
rm(combos$X)
combos <- combos[, 2, 7]
