# Load library
library(tidyverse)

# Read in breast cancer RNA-seq datacoun
counts = read_csv("GSE60450_GeneLevel_Normalized(CPM.and.TMM)_data.csv")
sampleInfo = read_csv("GSE60450_filtered_metadata.csv")

# view what is stored in variables
sampleInfo
counts

# dimension of variables-> rows by columns
dim(sampleInfo)
dim(counts)

# view the first 6 lines by default or specify more lines through Arg
head(sampleInfo)
?head # check Arg n
head(sampleInfo, 8)

# view the last 6 lines
tail(sampleInfo)

# view the whole variable
View(sampleInfo)# or just click on the variable in the Environment pane
View(counts)

# view column vectors
sampleInfo$X1
sampleInfo$immunophenotype

# view values from a to b [a:b] in a column vector
sampleInfo$X1[1:3]
sampleInfo$X1[2:4]
sampleInfo$immunophenotype[1:3]

# view the structure of the data
str(sampleInfo)
str(counts)

# summary of data: length of string vectors refers to num of coordinates
# whereas for numerical vectors: min, max, 1st quartile, 2nd quartile(median), 3rd quartile
summary(counts)
summary(sampleInfo)

# Excercises 1-4
# 1.
?head
head(sampleInfo, n = 8)
# 2.
subsetCounts = head(counts, n = 20)
# 3.
subsetCounts$GSM1480291
# 4.
mean(subsetCounts$GSM1480291)
# subsetting

# Formatting the data
seqData = pivot_longer(counts, col = starts_with('GSM'), names_to = 'Sample', values_to = 'Count')
seqData = pivot_longer(counts, col = GSM1480291:GSM1480302, names_to = 'Sample', values_to = 'Count')
seqData = pivot_longer(counts, col = -c('X1', 'gene_symbol'), names_to = 'Sample', values_to = 'Count')

allInfo = full_join(seqData, sampleInfo, by = c('Sample' = 'X1'))

# Plot data
ggplot(allInfo, mapping = aes(x = Sample, y = log2(Count + 1), colour = Sample)) + geom_violin()
ggplot(allInfo, mapping = aes(x = Sample, y = log2(Count + 1), fill = Sample)) + geom_violin()
ggplot(allInfo, mapping = aes(x = Sample, y = log2(Count + 1), fill = characteristics)) + geom_boxplot() 
ggplot(allInfo, mapping = aes(x = Sample, y = log2(Count + 1), fill = `developmental stage`)) + geom_boxplot() 

# Shorten Category names
allInfo = mutate(allInfo, Group = case_when(
  str_detect(characteristics, 'basal.*virgin') ~ 'bvirg',
  str_detect(characteristics, 'basal.*preg') ~ 'bpreg',
  str_detect(characteristics, 'basal.*lact') ~ 'blact',
  str_detect(characteristics, 'luminal.*virgin') ~ 'lvirg',
  str_detect(characteristics, 'luminal.*preg') ~ 'lpreg',
  str_detect(characteristics, 'luminal.*lact') ~ 'llact',
))

# Select 8 genes with the highest counts summed across all samples
myGenes = allInfo %>%
  group_by(gene_symbol) %>%
  summarise(Total_count = sum(Count)) %>%  # remove repeated values
  arrange(desc(Total_count)) %>% # Arrange data into descending order
  head(n = 8) %>%
  pull(gene_symbol) # Pull out a single variable

# Filter data
myGenesCounts = filter(allInfo, gene_symbol %in% myGenes)

# Create plot for each of the 8 genes
ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ gene_symbol)

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), fill = Group)) +
  geom_point() +
  facet_wrap(~ gene_symbol)

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol)

# customise plots
# colours
myColours = c('turquoise', 'plum', 'tomato', 'violet', 'steelblue', 'chocolate')

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  scale_colour_manual(values = myColours)

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  scale_colour_brewer(palette = 'Dark2')

# axis
ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'Count', title = 'Mammary gland RNA-seq data')

# theme
ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'log2(Count + 1)', title = 'Mammary gland RNA-seq data') +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'log2(Count + 1)', title = 'Mammary gland RNA-seq data') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'log2(Count + 1)', title = 'Mammary gland RNA-seq data') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = myGenesCounts, mapping = aes(x = Group, y = log2(Count + 1), colour = Group)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'log2(Count + 1)', title = 'Mammary gland RNA-seq data') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# order and categories(levels)
groupOrder = c('bvirg', 'bpreg', 'blact', 'lvirg', 'lpreg', 'llact')

myGenesCounts = mutate(myGenesCounts, Group_f = factor(Group, levels = groupOrder))

# seletively view data
myGenesCounts %>% select(X1, Group, Group_f)

str(myGenesCounts)

# check factor levels of a column
levels(myGenesCounts$Group_f)

# alter the order of data
ggplot(myGenesCounts, mapping = aes(x = Group_f, y = log2(Count + 1), colour = Group_f)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'log2(Count + 1)', title = 'Mammary gland RNA-seq data') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save the plot
pdf('myplot.pdf')

ggplot(myGenesCounts, mapping = aes(x = Group_f, y = log2(Count + 1), colour = Group_f)) +
  geom_jitter() +
  facet_wrap(~ gene_symbol) +
  labs(x = 'Cell type and stage', y = 'log2(Count + 1)', title = 'Mammary gland RNA-seq data') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dev.off()
