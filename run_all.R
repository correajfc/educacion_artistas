
library(tidyverse)
library(magrittr)
library(readxl)
library(janitor)
library(igraph)
library(ggraph)
library(wesanderson)
library(glue)
library(lubridate)
library(ggrepel)


# cargar datos  -----------------------------------------------------------


matriz_datos <- read_excel(path = "raw_data/matriz_dianaconcha.xlsx",
                           sheet = "Artistas",
                            range = cell_cols("A:U")) %>% clean_names()
    
artistas_titulos_instituciones <- matriz_datos %>% 
    select(nombre,ano_de_nacimiento,genero,pais,edad,institucion)

artistas_titulos_instituciones %<>% 
    mutate(institucion=str_replace_all(string = institucion," y ","-")) %>% 
    mutate(institucion=str_replace_all(institucion,"Kunsthonchschule-Weibensee Berlín","Kunsthonchschule Weibensee Berlín")) %>% 
    unnest(institucion = strsplit(institucion, "-")) %>% 
    drop_na()

artistas_titulos_instituciones %>% View()
    pull(institucion) %>% unique() %>% sort()

    nodos_institucion <-  artistas_titulos_instituciones %>% 
        select(nodo=institucion) %>% 
        distinct() %>% 
        mutate(rol="institución")
    
    nodos_artista <-  artistas_titulos_instituciones %>% 
        select(nodo=nombre) %>% 
        distinct() %>% 
        mutate(rol="artista")
    
    
    nodos<-bind_rows(nodos_institucion,nodos_artista)
    enlaces<-artistas_titulos_instituciones %>% 
        select(source=nombre,target=institucion) 
    
    
    
    
    grafoIA<-graph_from_data_frame(enlaces,
                                   directed = T, 
                                   vertices = nodos)
    
    
    # tipo<-if_else(V(grafoEA)$name %in% actividad.artistas[1:50,]$artista &
    #                   !(V(grafoEA)$name %in% los44),
    #               "relevante",
    #               V(grafoEA)$rol)
    
    label_nodes <-if_else(degree(grafoIA)>1,
                          V(grafoIA)$name,"")
    
    # tam_nodo<-if_else(V(grafoEA)$rol =="expos",5,degree(grafoEA))
    tam_nodo<-degree(grafoIA)
    p <- ggraph(grafoIA, layout = "fr", niter = 1500) + 
        geom_edge_link(
            # aes(width=peso_enlace),
            # arrow = arrow(length = unit(1, 'mm')), 
            # end_cap = circle(1, 'mm'),
            # start_cap=circle(1, 'mm'),
            edge_colour= "grey80") + 
        geom_node_point(aes(color = factor(rol), 
                            # size = degree(grafoEA)
                            size = tam_nodo
        )) +
        geom_node_text(aes(label = label_nodes),
                       repel = T 
                       # nudge_x = nchar(label_nodes)/10
        )+
        # coord_fixed()+
        scale_edge_width(range = c(0.1,1), guide = "none")+
        scale_color_manual(name ="Tipo nodo",values = wes_palette("Darjeeling1", n = length(V(grafoIA)$rol%>%unique())))+
        scale_size(name = "grado",
                   range = c(0.1,7)
        )+
        ggtitle('Artistas e instituciones educativas',
                subtitle = "Los nodos con nombre tienen más de un enalce")    
    
    p+theme_graph()     
    ggsave("g-artistas-institucione.svg",device = "svg", units = "cm", width = 40, height = 30 )
    ggsave("g-artistas-institucione.png",device = "png", units = "cm", width = 40, height = 30 )
    
    
    
    
    enlaces %>% 
        rename(institucion = target) %>% 
        group_by(institucion) %>% 
        count() %>% 
        arrange(desc(n)) %>% 
        rename(cantidad=n) %>% 
        write_csv("output_data/resumen_institucion.csv")
    
    enlaces %>% 
        rename(artista = source) %>% 
        group_by(artista) %>% 
        count() %>% 
        arrange(desc(n)) %>% 
        rename(cantidad=n) %>% 
        write_csv("output_data/resumen_artista_estudios.csv")
    
    matriz_datos %>% 
        mutate(fecha_nacimiento=ymd(glue("{ano_de_nacimiento}-1-1"))) %>% 
        mutate(exhibiciones_colectivas_cantidad=as.numeric(exhibiciones_colectivas_cantidad),
               exhibiciones_individuales_cantidad=as.numeric(exhibiciones_individuales_cantidad)) %>%
        replace_na(list(exhibiciones_individuales_cantidad = 0, 
                        exhibiciones_colectivas_cantidad = 0)) %>% 
        mutate(total_expos=exhibiciones_colectivas_cantidad+exhibiciones_individuales_cantidad) %>% 
        select(fecha_nacimiento,nombre,total_expos,genero,pais) %>% 
        write_csv("output_data/resumen_artista_expos.csv")
    
    
    matriz_datos %>% 
        mutate(fecha_nacimiento=ymd(glue("{ano_de_nacimiento}-1-1"))) %>% 
        mutate(exhibiciones_colectivas_cantidad=as.numeric(exhibiciones_colectivas_cantidad),
               exhibiciones_individuales_cantidad=as.numeric(exhibiciones_individuales_cantidad)) %>%
        replace_na(list(exhibiciones_individuales_cantidad = 0, 
                        exhibiciones_colectivas_cantidad = 0)) %>% 
        mutate(total_expos=exhibiciones_colectivas_cantidad+exhibiciones_individuales_cantidad) %>% 
        select(fecha_nacimiento,nombre,total_expos,genero,pais) %>% 
        mutate(nombre=if_else(total_expos>20,nombre,"")) %>% 
        drop_na() %>%
        ggplot()+
        geom_point(aes(x=fecha_nacimiento,
                       color=genero,
                       size = total_expos,
                       y =pais), position = "jitter",alpha = 0.7)+
        geom_text_repel(aes(x=fecha_nacimiento,
                            y =pais,
                            label = nombre),
                       repel = T, size =4 
                       # nudge_x = nchar(label_nodes)/10
        )+
        scale_x_date(name="fecha de nacimiento",
                     # date_breaks  = "10 years", 
                     date_labels = "%Y")+
        scale_size(name = "Cantidad exhibiciones",range = c(1,6))+
        theme_bw()+
        labs(title = "Fecha de nacimiento de los artistas, por género y país de nacimiento",
             subtitle = "Se nombran los artistas con más de 20 exposiciones")
        
    ggsave("artistas-expos-nacimiento.svg",device = "svg", units = "cm", width = 40, height = 40*0.62 )
    ggsave("artistas-expos-nacimiento.png",device = "png", units = "cm", width = 40, height = 40*0.62  )
    