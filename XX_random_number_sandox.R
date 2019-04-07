library(blockTools)
require(plyr)
require(dplyr)
#
###########

#create data frame elements
rep <- c("A") #start with rep A
plot <- c(1,2,3,4)  #no of plots
subplot <- c("E") #east side first
w <- c(0, 45, 65, 85) #severity levels
v <- c("T", "B")  #treatments

#create data frame
z <- data.frame(rep, plot, subplot)

#randomly assigns treatment and severity levels
z$treatment <- base::sample(v)
z$severity <- base::sample(w)

#create z.east that is the east side
z.east <- z

#west
z$subplot <- as.character(z$subplot)
z$subplot[z$subplot == "E"] <- "W"
z$treatment <- switch(z$treatment, "T" = "B", "B" = "T") 
z.east <- slice(z, sample(1:n()))
df2 <- sample_frac(z, 1L)
df2 <- transform(z, z$treatment = sample(w) )

#westside
y <- c("W")
z <- data.frame(plot, x, y, w)

colnames(z)<- c("replicate", "plot","subplot", "treatment")
z.west <- slice(z, sample(1:n()))



















# Load built-in dataset
data(HairEyeColor)
HairEyeColor <- data.frame(HairEyeColor)

# Transform so each row is a subject
# Columns describe subject's hair color, eye color, and gender
hec <- HairEyeColor[rep(1:nrow(HairEyeColor),
                        times = HairEyeColor$Freq), 1:3]

N <- nrow(hec)

# Fix the rownames
rownames(hec) <- NULL

# Set a seed for reproducability
set.seed(343)

# Create untreated and treated outcomes for all subjects
hec <- within(hec,{
  Y0 <- rnorm(n = N,mean = (2*as.numeric(Hair) + -4*as.numeric(Eye) + -6*as.numeric(Sex)), sd = 5)
  Y1 <- Y0 + 6*as.numeric(Hair) + 4*as.numeric(Eye) + 2*as.numeric(Sex)
})

# Calculate true ATE
with(hec, mean(Y1 - Y0))
#> [1] 25

# BlockTools requires that all variables be numeric
numeric_mat <- model.matrix(~Hair+Eye+Sex, data=hec)[,-1]

# BlockTools also requres an id variable
df_forBT <- data.frame(id_var = 1:nrow(numeric_mat), numeric_mat)

# Conducting the actual blocking: let's make trios
out <- block(df_forBT, n.tr = 3, id.vars = "id_var", 
             block.vars = colnames(df_forBT)[-1])

# Extact the block_ids
hec$block_id <- createBlockIDs(out, df_forBT, id.var = "id_var")

# Conduct actual random assignment with randomizr
Z_blocked <- block_ra(blocks = hec$block_id, num_arms = 3)
head(table(hec$block_id, Z_blocked))




###########
block_m_each <- 
  rbind(c(0, 0),
        c(45, 45),
        c(65, 65),
        c(85, 85))

x <- c(1,2,3,4)
y <- c("E","W")

z <- data.frame(expand.grid(x,y))

colnames(z)<- c("plot","subplot")





Z <- block_ra(blocks = z$plot,
              block_m_each = block_m_each)

table(hec$Hair, Z)