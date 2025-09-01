# What am I hoping to see here?
# Pubcount of Chinese researchers increase
# Is OpenAlex english pubs only? Yes
# Coauthorships with Chinese researchers increase??
# Obviously they're going to increase, they aren't just quitting academia?
# What I want to see is that the degree of connectedness and coauthorships between 1990 and 2006 goes up?
# But it seems like it's pretty obvious that that would happen too
# I want to see if there are subgroups that form within this network
# So I think at first those subgrouops will be defined by country/institution
# but over time they my shift and nation/institutional subgroups will become looser?


library("igraph")

researchers <- c("Chen Junshi", "Li Junyao", "Liu Boqi", "Colin T Campbell", "Richard Peto", "Martin Root", "Jillian Boreham", "Banoo Parpia", "Linda Youngman", "Pan Wenharn")
institutions90 <- c("Chinese Center for Disease Control and Prevention", "Chinese Academy of Medical Sciences", "Chinese Academy of Medical Sciences", "Cornell University", "Oxford University", "Cornell University", "Oxford University", "Cornell University", "Cornell University", "Academia Sinica" )
#institutions06
country <- c("China", "China", "China", "USA", "UK", "USA", "UK", "USA", "USA", "Taiwan")
#pubcount
# how do we do coauthorships as edges


ric90 <- data.frame(ID = 1:10, Researchers = researchers, Institutions_1990 = institutions90, Country = country, Publications = sample(10:300, nrow(ric90), replace = TRUE))


# This feels like something that I might need to do? Idk
#setClass("Researcher", slots = c(name = "character", institutions90 = "character", institutions06 = "character", country = "character", pubcount = "numeric", coauthorships = "numeric"))

# List of IDs
id_list <- 1:10

# Generate all combinations of two IDs
id_combinations <- combn(id_list, 2)

# Convert the matrix to a data frame
id_combinations_df <- as.data.frame(t(id_combinations))

# Rename the columns
colnames(id_combinations_df) <- c("ID1", "ID2")

#currently using a random number for coauthorships until I can get real data
id_combinations_df$coauthorships <- sample(1:100, nrow(id_combinations_df), replace = TRUE)

# net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
# links would be a column of coaurthorships? ID to ID


ric90_net <- graph_from_data_frame(d = id_combinations_df, vertices = ric90, directed = F )

ric90_net 

as_adjacency_matrix(ric90_net)
#I don't think my edges are working how I want them to

# Assign random colors to each country
country_colors <- sample(rainbow(length(ric90$Country)))

# Create a named vector to map countries to colors
country_color_mapping <- setNames(country_colors, ric90$Country)

# Assign vertex colors based on the country_color_mapping
vertex_colors <- country_color_mapping[ric90$Country]

# Set node size based on pub count:
V(ric90_net)$size <- V(ric90_net)$Publications*0.7

# Set edge length based on coauthorships
E(ric90_net)$length <- E(ric90_net)

plot(ric90_net, vertex.color = vertex_colors, 
     vertex.frame.colors = vertex_colors, 
     vertex.shape = "circle", 
     vertex.label = ric90$Researchers,
     vertex.frame.color="#555555",
     vertex.label.color="black")

