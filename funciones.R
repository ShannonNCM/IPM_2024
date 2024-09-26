#funcion para ingresar los datos
# esta funcion me permite que el script corra de forma dinamica
inputdata <- function(month_number) {
  month_names <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  if (month_number < 1 || month_number > 12) {
    stop("Por favor ingresar numero entre 1 y 12.")
  }
  
  mes <- month_names[month_number]
  
  colstorename <- 2:(month_number + 1)
  
  months <- month_names[1:month_number]
  
  col_indices <- c(1, seq(2, 2 * month_number, by = 2))
  
  return(list(mes = mes, colstorename = colstorename, months = months, col_indices = col_indices))
}


#funcion para asignar los codigos a los distintos niveles
assigncod <- function(x){
  x %>% 
    mutate(division = substr(Codigo, 1, 1), .before = Codigo) %>% 
    mutate(clase = substr(Codigo, 1, 2), .before = Codigo) %>% 
    mutate(subclase = substr(Codigo, 1, 3), .before = Codigo) %>% 
    mutate(grupo = substr(Codigo, 1, 4), .before = Codigo) %>% 
    mutate(producto = substr(Codigo, 1, 5), .before = Codigo)
}

#funcion para calcular el indice a nivel variedad
var_ind <- function(x, col_name){
  col_sym <- sym(col_name) #esto convierte el nombre de la columna a un simbolo
  x %>% 
    drop_na(!!col_sym) %>% #unquotes a variable or symbol so that its value is evaluated within the context of a dplyr function
    mutate(const = PONDERACION/PRECIOBASE) %>% 
    mutate(particp = const * !!col_sym) %>% 
    mutate(ind = (particp/PONDERACION)*100)
}

#funcion para calcular el indice por producto, grupo, sublcase, clase y division
ind <- function(x, col_name){
  col_sym <- ensym(col_name)
  
  x %>% group_by(!!col_sym) %>% 
    summarize(pon_prod = sum(PONDERACION),
              part_prod = sum(particp)) %>% 
    mutate(ind = (part_prod/pon_prod)*100) %>% 
    rename(Codigo = 1) %>% 
    left_join(ponderaciones, by = join_by(Codigo)) %>% 
    select(PRODUCTOS, ind)
}

#funcion para generar el excel resumen de los indices para cada mes
exp_excel <- function(excel, mes, indvar, indprod, indgrupo, indsubclase, indclase, inddiv, indgen){
  
  wb <- createWorkbook()
  
  datos <- list("ind_var" = indvar, "ind_prod" = indprod, "ind_grupo" = indgrupo, 
                "ind_subclase" = indsubclase, "ind_clase" = indclase, "ind_div" = inddiv, "ind_gen" = indgen)
  
  for (sheet in names(datos)) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, datos[[sheet]])
  }
  
  nombre <- paste0(excel, "_", mes, ".xlsx")
  
  saveWorkbook(wb, nombre, overwrite = TRUE)
}

#funcion para leer los excel resumen y generar el "informe con todos los meses"
readind <- function(months, sheet_names){
  dflist <- vector("list", length(months))
  
  for (i in seq_along(months)) {
    file_path <- paste0("resumen/resumen_ipm_", months[i], ".xlsx")
    sheet_name <- sheet_names[i]
    dflist[[i]] <- read_excel(file_path, sheet = sheet_name)
  }
  return(dflist)
}
