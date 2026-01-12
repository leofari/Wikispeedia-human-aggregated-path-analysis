#Final Project Raw R file
library(readr)
library(dplyr)
library(igraph)
library(tidyr)
library(ggplot2)

#load "links" file to get the pages which are connected by a hyperlink

true_links = read_table(
  "wikispeedia_paths-and-graph/links.tsv",
  comment = "#",
  col_names = c("source", "target")
)

#Remove the url encoding on both sides
true_links = true_links %>%
  mutate(
    source = URLdecode(source),
    target = URLdecode(target)
  )

#The graph connecting pages that share a hyperlink
true_graph = graph_from_data_frame(true_links)



#Read csv connecting articles to catagories
art_cat_edges = read_tsv(
  "wikispeedia_paths-and-graph/categories.tsv",
  comment = "#",
  col_names = c("article", "category")
) %>%
  mutate(
    article  = URLdecode(article)
  )

#set up as links between an article and a catagory
art_cat_edges = art_cat_edges %>% 
  mutate(category_path = sub("^subject\\.", "", category)) %>%
  select(-category) %>%
  separate_rows(category_path, sep = "\\.") %>%
  rename(category = category_path)

#Set up bipartite proj for cat-cat links

cat_cat_edges = art_cat_edges %>%
  inner_join(art_cat_edges, by = "article") %>%     
  filter(category.x < category.y) %>%           
  count(category.x, category.y, name = "weight") %>%
  rename(cat1 = category.x, cat2 = category.y)

cat_graph = graph_from_data_frame(cat_cat_edges, directed = FALSE)


# set up human


human_paths = read_tsv(
  "wikispeedia_paths-and-graph/paths_finished.tsv",
  comment = "#",
  col_names = c("hash", "timestamp", "duration", "path", "rating"),
  col_types = cols(.default = col_character())
)

human_paths = human_paths %>% select(path)



# turn one path string into directed edges
edges_from_path = function(path_string) {
  # split actions (articles or <)
  actions = strsplit(path_string, ";", fixed = TRUE)[[1]]
  if (length(actions) < 2) return(data.frame(from=character(0), to=character(0)))
  # url decode actions that are article names 
  for (i in seq_along(actions)) {
    if (actions[i] != "<") actions[i] = URLdecode(actions[i])
  }
  
  # stack represents the browsing history (top = current page)
  stack = character(0)
  
  # initialize: first action should be a page
  if (actions[1] == "<") return(data.frame(from=character(0), to=character(0)))
  stack = c(stack, actions[1])
  
  froms = character(0)
  tos   = character(0)
  
  # process each action as a "move" from current -> next
  for (i in 2:length(actions)) {
    current = stack[length(stack)]
    
    if (actions[i] == "<") {
      # back click: go to previous page if it exists
      if (length(stack) >= 2) {
        stack = stack[-length(stack)]      
        next_page = stack[length(stack)]   
        froms = c(froms, current)
        tos   = c(tos, next_page)
      }
      # if no previous page, ignore the <
    } else {
      next_page = actions[i]
      froms = c(froms, current)
      tos   = c(tos, next_page)
      stack = c(stack, next_page)          
    }
  }
  
  data.frame(from = froms, to = tos, stringsAsFactors = FALSE)
}

# helper function to get the quit page and second to last page before quit

quit_transition_from_path = function(path_string) {
  actions = strsplit(path_string, ";", fixed = TRUE)[[1]]
  if (length(actions) < 2)
    return(c(NA_character_, NA_character_))
  
  for (i in seq_along(actions)) {
    if (actions[i] != "<") actions[i] = URLdecode(actions[i])
  }
  
  if (actions[1] == "<")
    return(c(NA_character_, NA_character_))
  
  stack = character(0)
  stack = c(stack, actions[1])
  
  prev = NA_character_
  curr = actions[1]
  
  for (i in 2:length(actions)) {
    if (actions[i] == "<") {
      if (length(stack) >= 2) {
        curr = stack[length(stack)]
        stack = stack[-length(stack)]
        prev = curr
        curr = stack[length(stack)]
      }
    } else {
      prev = curr
      curr = actions[i]
      stack = c(stack, curr)
    }
  }
  
  c(prev, curr)
}



paths_unfinished = read_tsv(
  "wikispeedia_paths-and-graph/paths_unfinished.tsv",
  comment = "#",
  col_names = c("hash", "timestamp", "duration", "path", "rating"),
  col_types = cols(.default = col_character())
)


quit_transitions = paths_unfinished %>%
  mutate(tmp = lapply(path, quit_transition_from_path)) %>%
  mutate(
    prev_page = vapply(tmp, `[`, character(1), 1),
    quit_page = vapply(tmp, `[`, character(1), 2)
  ) %>%
  filter(!is.na(quit_page)) %>%
  count(prev_page, quit_page, name = "quit_count")




# build full directed list
all_edges = data.frame(from=character(0), to=character(0))


#this for loop takes a while to run 💀
for (k in 1:nrow(human_paths)) {
  e = edges_from_path(human_paths$path[k])
  e2 = edges_from_path(paths_unfinished$path[k])
  if (nrow(e) > 0) all_edges = rbind(all_edges, e)
  if (nrow(e2) > 0) all_edges = rbind(all_edges, e2)
}


# collapse to weights 
human_edges = all_edges %>%
  count(from, to, name = "weight")


human_graph = graph_from_data_frame(human_edges, directed = TRUE)
E(human_graph)$weight = human_edges$weight






#Now begins phase 2: We have all of our graphs, time to perform analyses.


articles = read_table(
  "wikispeedia_paths-and-graph/articles.tsv",
  comment = "#",
  col_names = c("article")
)

articles = articles %>% mutate(article = URLdecode(article))

#
art_to_cat = art_cat_edges %>% distinct(article, category)
links = true_links %>% distinct(source, target)


# 0-click connectivity (number of categories A belongs to)
conn_0 = art_to_cat %>%
  count(article, name = "conn_0")


# 0-click frequency: by convention (no click) you “stay” 100%
freq_0 = articles %>%
  transmute(article, freq_0 = 1)

# 1-click connectivity: average number of categories of B over links A->B
b_cat_counts = art_to_cat %>%
  count(article, name = "b_num_cats")

conn_1 = links %>%
  left_join(b_cat_counts, by = c("target" = "article")) %>%
  group_by(source) %>%
  summarise(conn_1 = mean(b_num_cats, na.rm = TRUE), .groups = "drop") %>%
  mutate(conn_1 = ifelse(is.nan(conn_1), NA_real_, conn_1)) %>%
  rename(article = source)

# 1-click frequency, fraction of outgoing clicks which stay in A 
freq_1 = links %>%
  left_join(art_to_cat, by = c("source" = "article")) %>%
  rename(source_cat = category) %>%
  left_join(art_to_cat, by = c("target" = "article")) %>%
  rename(target_cat = category) %>%
  mutate(same_cat = source_cat == target_cat) %>%
  group_by(source, target) %>%
  summarise(any_shared = any(same_cat), .groups = "drop") %>%
  group_by(source) %>%
  summarise(freq_1 = mean(any_shared), .groups = "drop") %>%
  rename(article = source)

paths_2 = links %>%
  rename(mid = target) %>%
  inner_join(links, by = c("mid" = "source")) %>%
  rename(start = source, end = target)   # start = A, end = C

# 2-click connectivity: average number of categories of C over links A->B->C
c_cat_counts = art_to_cat %>%
  count(article, name = "c_num_cats")

conn_2 = paths_2 %>%
  left_join(c_cat_counts, by = c("end" = "article")) %>%
  group_by(start) %>%
  summarise(conn_2 = mean(c_num_cats, na.rm = TRUE), .groups = "drop") %>%
  mutate(conn_2 = ifelse(is.nan(conn_2), NA_real_, conn_2)) %>%
  rename(article = start)

# 2-click frequency: for links A->B->C, the percentage of the time C will have the
# same category as A
freq_2 = paths_2 %>%
  left_join(art_to_cat, by = c("start" = "article")) %>%
  rename(start_cat = category) %>%
  left_join(art_to_cat, by = c("end" = "article")) %>%
  rename(end_cat = category) %>%
  mutate(same_cat = start_cat == end_cat) %>%
  group_by(start, end) %>%
  summarise(any_shared = any(same_cat), .groups = "drop") %>%
  group_by(start) %>%
  summarise(freq_2 = mean(any_shared), .groups = "drop") %>%
  rename(article = start)

articles = articles %>%
  left_join(conn_0, by = "article") %>%
  left_join(freq_0, by = "article") %>%
  left_join(conn_1, by = "article") %>%
  left_join(freq_1, by = "article") %>% 
  left_join(conn_2, by = "article") %>%
  left_join(freq_2, by = "article")



human_probs = human_edges %>%
  group_by(from) %>%
  mutate(out_w = sum(weight), prob = weight / out_w) %>%
  ungroup()

conn_1_human = human_probs %>%
  left_join(b_cat_counts, by = c("to" = "article")) %>%
  group_by(from) %>%
  summarise(
    conn_1_human = sum(prob * b_num_cats, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(article = from)

freq_1_human = human_probs %>%
  left_join(art_to_cat, by = c("from" = "article")) %>%
  rename(from_cat = category) %>%
  left_join(art_to_cat, by = c("to" = "article")) %>%
  rename(to_cat = category) %>%
  mutate(same_cat = from_cat == to_cat) %>%
  group_by(from, to, prob) %>%
  summarise(any_shared = any(same_cat), .groups = "drop") %>%
  group_by(from) %>%
  summarise(
    freq_1_human = sum(prob * any_shared),
    .groups = "drop"
  ) %>%
  rename(article = from)

paths_2_human = human_probs %>%
  select(start = from, mid = to, p1 = prob) %>%
  inner_join(
    human_probs %>% select(from, end = to, p2 = prob),
    by = c("mid" = "from")
  ) %>%
  mutate(p_path = p1 * p2)

conn_2_human = paths_2_human %>%
  left_join(c_cat_counts, by = c("end" = "article")) %>%
  group_by(start) %>%
  summarise(
    conn_2_human = sum(p_path * c_num_cats, na.rm = TRUE) / sum(p_path[!is.na(c_num_cats)]),
    .groups = "drop"
  ) %>%
  rename(article = start)

freq_2_human = paths_2_human %>%
  left_join(art_to_cat, by = c("start" = "article")) %>%
  rename(start_cat = category) %>%
  left_join(art_to_cat, by = c("end" = "article")) %>%
  rename(end_cat = category) %>%
  mutate(same_cat = start_cat == end_cat) %>%
  group_by(start, end, p_path) %>%
  summarise(any_shared = any(same_cat), .groups = "drop") %>%
  group_by(start) %>%
  summarise(
    freq_2_human = sum(p_path * any_shared) / sum(p_path),
    .groups = "drop"
  ) %>%
  rename(article = start)


articles_human = articles %>%
  left_join(conn_1_human, by = "article") %>%
  left_join(freq_1_human, by = "article") %>%
  left_join(conn_2_human, by = "article") %>%
  left_join(freq_2_human, by = "article")



cat_walktrap = cluster_walktrap(cat_graph)


m = membership(cat_walktrap)

n = vcount(cat_graph)
L = layout_with_fr(cat_graph)

w = E(cat_graph)$weight
w_scaled = 1 + 20 * (w - min(w)) / (max(w) - min(w))  

mean(degree(cat_graph))

plot(cat_graph,
     layout = L,
     vertex.size = 10,
     vertex.color = as.factor(m),
     vertex.frame.color = "gray30",
     vertex.label.cex = .4,
     edge.width = w_scaled,
     vertex.label.font = 2,
     vertex.shape = "rectangle",
     vertex.label.color = "black")












articles_human = articles_human %>%
  rename(
    conn_0_struct = conn_0,
    retention_0_struct = freq_0,
    
    conn_1_struct = conn_1,
    retention_1_struct = freq_1,
    
    conn_2_struct = conn_2,
    retention_2_struct = freq_2,
    
    conn_1_human = conn_1_human,
    retention_1_human = freq_1_human,
    
    conn_2_human = conn_2_human,
    retention_2_human = freq_2_human
  )

summary(articles_human)



#Catagory - catagory plots

ggplot(articles_human, aes(x = conn_1_struct, y = retention_1_struct)) +
  geom_point(alpha = 0.08, size =1) +
  labs(title = "Reten_1 vs conn_1")

ggplot(articles_human, aes(x = conn_2_struct, y = retention_2_struct)) +
  geom_point(alpha = 0.08) +
  labs(title = "Reten_2 vs conn_2")


#Distribution of struct
articles_human %>%
  pivot_longer(cols = c(conn_0_struct, conn_1_struct, conn_2_struct, retention_1_struct, retention_2_struct),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 60) +
  facet_wrap(~metric, scales = "free") +
  labs(title = "Category metrics: distributions")


#dist human vs graph retention
articles_human %>%
  pivot_longer(
    cols = c(retention_1_struct, retention_2_struct, retention_1_human, retention_2_human),
    names_to = "metric", values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "steelblue") +
  facet_wrap(~metric) +
  labs(title = "Category metrics: distributions", y = "Density")

#dist human vs graph connectivity
articles_human %>%
  pivot_longer(
    cols = c(conn_1_struct, conn_2_struct, conn_1_human, conn_2_human),
    names_to = "metric", values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "steelblue") +
  facet_wrap(~metric) +
  labs(title = "Category metrics: distributions", y = "Density")




#Human plots

df_h = articles_human %>% filter(!is.na(retention_1_human), !is.na(conn_1_human),
                                 !is.na(retention_2_human), !is.na(conn_2_human))

#Do category frequencies predict human frequencies?

ggplot(df_h, aes(x = conn_1_struct, y = retention_1_human)) +
  geom_point(alpha = 0.08, size = 1) +
  labs(title = "Human vs category: freq_1_human vs freq_1",
       x = "conn_1_struct (category/structural)", y = "retention_1_human (human)")


df_dif = df_h %>%
  mutate(
    d_reten1 = retention_1_human - retention_1_struct,
    d_reten2 = retention_2_human - retention_2_struct,
    d_conn1 = conn_1_human - conn_1_struct,
    d_conn2 = conn_2_human - conn_2_struct
  )

df_dif %>%
  select(d_reten1, d_reten2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "difference"
  ) %>%
  mutate(
    n = ifelse(metric == "d_reten1", "n = 1", "n = 2")
  ) %>%
  ggplot(aes(x = difference)) +
  geom_histogram(bins = 70, fill = "steelblue") +
  facet_wrap(~n, scales = "fixed") +
  labs(
    title = "Human − Structural Retention Differences",
    x = "Retention difference",
    y = "Count"
  )

df_dif %>%
  select(d_conn1, d_conn2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "difference"
  ) %>%
  mutate(
    n = ifelse(metric == "d_conn1", "n = 1", "n = 2")
  ) %>%
  ggplot(aes(x = difference)) +
  geom_histogram(bins = 70) +
  facet_wrap(~n, scales = "fixed") +
  labs(
    title = "Human − Structural Connectivity Differences",
    x = "Connectivity difference",
    y = "Count"
  )




#How is each category spread out?

cat_stats = art_to_cat %>%
  inner_join(df_h, by = "article") %>%
  group_by(category) %>%
  summarise(
    n_articles = n_distinct(article),
    mean_freq1 = mean(retention_1_struct, na.rm = TRUE),
    mean_freq2 = mean(retention_2_struct, na.rm = TRUE),
    mean_conn2 = mean(conn_2_struct, na.rm = TRUE),
    mean_freq1_h = mean(retention_1_human, na.rm = TRUE),
    mean_conn2_h = mean(conn_2_human, na.rm = TRUE),
    .groups = "drop"
  )


# i like this one (shows 25 biggest catagories for freq_1)
cat_stats %>%
  arrange(desc(n_articles)) %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(category, mean_freq1), y = mean_freq1)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 25 categories by mean structural freq_1",
       x = "category", y = "mean freq_1")

#25 biggest catagories for freq_2
cat_stats %>%
  arrange(desc(n_articles)) %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(category, mean_freq2), y = mean_freq2)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 25 categories by mean structural freq_1",
       x = "category", y = "mean freq_2")


## Failed paths

K = 500

quit_top = quit_transitions %>%
  arrange(desc(quit_count)) %>%
  slice_head(n = K)

# Collapse to "page-level": how often a page is a quit page / prev page
fail_pages = bind_rows(
  quit_top %>%
    transmute(article = quit_page, role = "quit_page", w = quit_count),
  quit_top %>%
    transmute(article = prev_page, role = "prev_page", w = quit_count)
) %>%
  group_by(role, article) %>%
  summarise(w = sum(w), .groups = "drop") %>%
  left_join(articles_human, by = "article")



#Quit pages vs All — Retention overlay


metrics_ret = c("retention_1_struct", "retention_2_struct",
                "retention_1_human",  "retention_2_human")

bind_rows(
  articles_human %>%
    select(all_of(metrics_ret)) %>%
    mutate(group = "All articles", w = 1),
  
  fail_pages %>%
    filter(role == "quit_page") %>%
    select(all_of(metrics_ret), w) %>%
    mutate(group = "Quit pages (weighted)")
) %>%
  pivot_longer(cols = all_of(metrics_ret), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, weight = w, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, position = "identity", alpha = 0.65) +
  facet_wrap(~metric, ncol = 2) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = paste0("Retention: Quit pages vs All (top ", K, " quit transitions)"),
    x = "Retention",
    y = "Density",
    fill = "Group"
  )

#Prev pages vs All — Retention overlay


bind_rows(
  articles_human %>%
    select(all_of(metrics_ret)) %>%
    mutate(group = "All articles", w = 1),
  
  fail_pages %>%
    filter(role == "prev_page") %>%
    select(all_of(metrics_ret), w) %>%
    mutate(group = "Prev pages (weighted)")
) %>%
  pivot_longer(cols = all_of(metrics_ret), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, weight = w, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, position = "identity", alpha = 0.65) +
  facet_wrap(~metric, ncol = 2) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = paste0("Retention: Prev pages vs All (top ", K, " quit transitions)"),
    x = "Retention",
    y = "Density",
    fill = "Group"
  )

#Quit pages vs All — Connectivity overlay

metrics_conn = c("conn_1_struct", "conn_2_struct",
                 "conn_1_human",  "conn_2_human")

bind_rows(
  articles_human %>%
    select(all_of(metrics_conn)) %>%
    mutate(group = "All articles", w = 1),
  
  fail_pages %>%
    filter(role == "quit_page") %>%
    select(all_of(metrics_conn), w) %>%
    mutate(group = "Quit pages (weighted)")
) %>%
  pivot_longer(cols = all_of(metrics_conn), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, weight = w, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, position = "identity", alpha = 0.65) +
  facet_wrap(~metric, ncol = 2) +
  labs(
    title = paste0("Connectivity: Quit pages vs All (top ", K, " quit transitions)"),
    x = "Connectivity",
    y = "Density",
    fill = "Group"
  )


#Prev pages vs All — Connectivity overlay
bind_rows(
  articles_human %>%
    select(all_of(metrics_conn)) %>%
    mutate(group = "All articles", w = 1),
  
  fail_pages %>%
    filter(role == "prev_page") %>%
    select(all_of(metrics_conn), w) %>%
    mutate(group = "Prev pages (weighted)")
) %>%
  pivot_longer(cols = all_of(metrics_conn), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, weight = w, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, position = "identity", alpha = 0.65) +
  facet_wrap(~metric, ncol = 2) +
  labs(
    title = paste0("Connectivity: Prev pages vs All (top ", K, " quit transitions)"),
    x = "Connectivity",
    y = "Density",
    fill = "Group"
  )

#top 25 quit pages by structural retention
top_quit_pages = quit_transitions %>%
  group_by(quit_page) %>%
  summarise(total_quits = sum(quit_count), .groups = "drop") %>%
  arrange(desc(total_quits)) %>%
  slice_head(n = 25)

top_quit_pages %>%
  left_join(articles_human, by = c("quit_page" = "article")) %>%
  ggplot(aes(
    x = reorder(quit_page, retention_2_struct),
    y = retention_2_struct
  )) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 most-quit pages by structural retention (n = 2)",
    x = "Article",
    y = "Mean structural retention₂"
  )

#top 25 quits by total count
top_quit_pages %>%
  ggplot(aes(
    x = reorder(quit_page, total_quits),
    y = total_quits
  )) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 25 most-quit pages by total quit count",
    x = "Article",
    y = "Number of quits"
  )

