# función sacada de kuzco package
my_view_llm_results <- function(llm_results) {
  llm_results_long <- llm_results |>
    dplyr::mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "Contexto",
      values_to = "Respuesta"
    ) |>
    dplyr::mutate(
      Contexto = dplyr::recode(
        Contexto,
        image_classification = "Clasificación de la Imagen",
        primary_object = "Objeto Primario",
        secondary_object = "Objeto Secundario",
        image_description = "Descripción de la Imagen",
        image_colors = "Colores de la Imagen",
        image_proba_names = "Nombres Probables en la Imagen",
        image_proba_values = "Probabilidades Asociadas"
      )
    ) |>
    gt::gt() |>
    gt::tab_header(
      title = "Visión por computadora"
    ) |>
    gt::tab_options(
      column_labels.background.color = "#B71234"
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels()) |>
    gt::tab_style(
      style = gt::cell_text(
        color = '#B71234',
        weight = 'bold'
      ),
      locations = gt::cells_title(groups = 'title')
    )
}
