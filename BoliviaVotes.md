Bolivia Vota
================

This document summarizes our attempts at finding voting tampering in the
recent Bolivian elections. The datasets were obtained via the official
polling authority Organo Electoral Plurinacional (OEP) at the website
`http://computo.oep.org.bo`.

For this purpose, we are considering two datasets:

  - Results of the rapid counting process obtained on Sunday 22, October
    2019 at 20:38:53 (GMT-4) that we shall call **TREP** sample.
  - Results of the computations Friday 25 October 2019 at
    01:40:43(GMT-4), which we shall call the **PRES** sample.

In what follows, we shall focus on the results for the election for
President and Vicepresident of the Plurinational State.

## 1\. Comparison between Valid Votes and Polling Booth aggregation.

As a first exercise, we consider the **PRES** sample. We aim to compare
the following variables:

  - `Votos.Válidos` : The total amount of votes per polling booth.
  - The aggregate of the votes for the political parties in the booth,
    that we shall store in a new variable called `Votos.Partidos`.

Our hypothesis is that booths that show divergence between these two
quantities are suspicious of being tampered, as their difference
(`Votos.Partidos - Votos.Validos`) is expected to be zero. We shall
identify these booths in the variable `Sospecha`

The conclusions of this first exploration are as follows:

  - A total of 919 booths were deemed as supicious, which represents
    2.71% of the total number of booths.  
  - A total of 1.033810^{4} were miscounted, be it in the positive or
    negative sense. That is 0.16% of the total amount of votes.
  - These divergences range in the following way:

<!-- end list -->

``` r
knitr::kable(Resumen.Diferencia.Votos)
```

|  | Diferencia.Partidos |
|  | :------------------ |
|  | Min. :-109.00000    |
|  | 1st Qu.: -8.00000   |
|  | Median : -2.00000   |
|  | Mean : -0.05005     |
|  | 3rd Qu.: 1.00000    |
|  | Max. : 209.00000    |

``` r
actas_pres %>% 
  filter(Sospecha=="Sospechosa")  %>%
  select(Diferencia.Partidos) %>%
  summary()
```

    ##  Diferencia.Partidos 
    ##  Min.   :-109.00000  
    ##  1st Qu.:  -8.00000  
    ##  Median :  -2.00000  
    ##  Mean   :  -0.05005  
    ##  3rd Qu.:   1.00000  
    ##  Max.   : 209.00000

### Results

``` r
actas_pres_tidy <- actas_pres %>% melt(
  measure.vars  = 
    c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL"),
  variable_name= "Partido")
```

``` r
actas_trep <- read.csv("./datasets/acta.2019.10.22.20.38.53.csv")

actas_pres_trep <- actas_trep %>%  
  filter(
    Elección  == "Presidente y Vicepresidente"
    ) 

actas_pres_trep$Votos.Partidos <- 
  actas_pres_trep[,partidos] %>% 
  rowSums()

actas_pres_trep$Votos.Emitidos <- 
  actas_pres_trep[,emitidos] %>% 
  rowSums()

actas_pres_trep$Diferencia.Partidos <- 
  actas_pres_trep$Votos.Partidos - actas_pres_trep$Votos.Válidos

actas_pres_trep %>% 
  mutate(
    Sospecha = ifelse(
      Diferencia.Partidos!=0,"Sospechosa","Normal"),
    Sampl = "trep"
    ) -> actas_pres_trep
```

33901 33044

## Percieved Sospechaularities

``` r
actas_pres_trep$Diferencia.Partidos <- 
  actas_pres_trep$Votos.Partidos - actas_pres_trep$Votos.Válidos

actas_pres_trep %>% 
  mutate(
    Sospecha= ifelse(
      Diferencia.Partidos!=0,"Sospechosa","Normal") 
    ) -> actas_pres

## 

actas_pres_trep %>% 
  filter(Sospecha=="Sospechosa")  %>%
  select(Diferencia.Partidos) %>%
  summary()
```

    ##  Diferencia.Partidos
    ##  Min.   :-115.000   
    ##  1st Qu.:  -9.000   
    ##  Median :  -3.000   
    ##  Mean   :  -1.552   
    ##  3rd Qu.:   1.000   
    ##  Max.   : 213.000

``` r
actas_pres_trep %>% 
  filter(Sospecha=="Sospechosa")  %>%
  select(Diferencia.Partidos) %>%
  abs() %>%
  sum() 
```

    ## [1] 17091

``` r
actas_pres_trep %>% 
  select(Votos.Emitidos) %>% 
  sum() 
```

    ## [1] 6194344

``` r
actas_trep_tidy <- actas_pres_trep %>% melt(
  measure.vars  = 
    c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL"),
  variable_name= "Partido")
```

## 2\. Overviewing voting per Polling Districts

In this part, we try to check whether there is a difference between the
results of voting at each booth within a precint and the results of the
precint. One would expect that the distribution of the former is somehow
reflective of the latter. This can be assessed graphically or via
testing procedures with the respective corrections.

However, booths with a small amount of voters:

1.  Could have a problem reflecting the distribution of the precint as
    they are small samples.
2.  Hinder the statistical testing process.

To adress the first

``` r
ComoVotaMiRecinto <- function(recinto, porcentajes =FALSE){
   if(!(recinto %in% levels(actas$Recinto))) {
     stop(message("Nombre de recinto no valido."))
   } else {
     data <- actas_pres %>% melt(
       measure.vars  = partidos,
       variable_name= "Partido") %>%
       mutate(Porcentaje.por.Partido =  value/Votos.Emitidos*100) %>%
       filter(
         Recinto  == recinto) %>%
       group_by(Número.Mesa, Partido)
       
       if(!porcentajes){
        plots <- data %>% 
          ggplot(mapping = aes(x = Partido, 
                               y=value, 
                               fill = factor(Número.Mesa))) +
          geom_col(position ="dodge")+
          facet_grid(.~Departamento+Provincia+Localidad+Sospecha, 
                     scales = "free_y") + 
          ggtitle(paste("Localidad :", recinto, "- Total")) + 
          labs(x = "Partido", y = "Total de Votos")
       } else if (porcentajes){
         plots <- data %>% 
           ggplot(mapping = aes(x = Partido, 
                                y = Porcentaje.por.Partido, 
                                fill = factor(Número.Mesa))) +
           geom_col(position ="dodge")+
           facet_grid(.~Departamento+Provincia+Localidad+Sospecha, scales = "free_y") + 
           ggtitle(paste("Localidad :", recinto, "- Porcentaje por Mesa"))+ 
           labs(x = "Partido", y = "Porcentaje de Votos")
       }
     plots
   }
}
```

### Some Examples

![](BoliviaVotes_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](BoliviaVotes_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->
