arrange_tree <- function(periores, ext_inf){
  # perio_res : object return by SPARTAAS::perioclust (with alpha and k fixed)
  # ext_inf : a tibble with the following column
  #  - id : the labels returned by perio_res$tree$labels
  #  - x (scalar) : the information used to rearrange the tree
  
  
  inf_cluster <- tibble::tibble(
    id = periores$cluster %>% names(),
    clust = periores$cluster %>% as.numeric()
  ) %>% dplyr::inner_join(ext_inf, by = "id") %>%
    dplyr::group_by(clust) %>%
    dplyr::mutate(mx = mean(x)) %>%
    dplyr::arrange(mx,x)
  nclust <- inf_cluster %>%
    dplyr::pull(clust) %>%
    max()
  dend_arranged <- periores$tree %>%
    as.dendrogram() %>%
    dendextend::rotate(inf_cluster %>%
                         dplyr::pull(id)) %>%
    dendextend::color_branches(k = nclust , groupLabels = TRUE)
  return(dend_arranged)
}