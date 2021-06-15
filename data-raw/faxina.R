# Lendo o banco de dados

ipca<-readxl::read_excel("data-raw/IPCA MENSAL 2007-2019.xlsx")
dplyr::glimpse(ipca)



ipca |> 
  dplyr::mutate(Cadeia = if_else(Cadeia == "Suinoculltura","Suinocultura",Cadeia) ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("0"),
    names_to = "data",
    values_to = "var_ipca"
    ) |> 
  dplyr::mutate(
    data=lubridate::dmy(stringr::str_remove_all(data,"_"))
  ) |> 
  tidyr::drop_na() |> 
  dplyr::group_by(Cadeia) |> 
  dplyr::filter(Cadeia=="Bovinocultura") |> 
  ggplot2::ggplot(ggplot2::aes(x=data,y=var_ipca,color=Alimento) ) +
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~Process.) +
  ggplot2::theme_minimal()
  
ipca <- ipca |> 
  dplyr::mutate(
    Cadeia = if_else(Cadeia == "Suinoculltura","Suinocultura",Cadeia) 
    ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("0"),
    names_to = "data",
    values_to = "var_ipca"
  ) |> 
  dplyr::mutate(
    data=lubridate::dmy(stringr::str_remove_all(data,"_"))
  )
readr::write_rds(ipca, "data/ipca.rds")

# para cada gráfico, 
# uma estatistica descritiva - média móvel
# Análise de correlação
# Análise de correlograma em pares



