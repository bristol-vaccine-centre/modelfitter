# devtools::load_all("~/Git/tableone")
# devtools::load_all("~/Git/modelfitter")

#' Forest plot from a set of bootstrapped models
#'
#' @param boots a set of bootstrapped model fits as output by `run_models`
#' @param facet a faceting variable (usually `modelName``)
#' @param stat_label what to call the x axis
#' @param report_fit which components of the model performance statistics do we
#'   want to report
#' @param limit x axis limits
#' @param p.component indicate which component parts of the fit are significant
#'   and which are not
#' @param label_fn a function (or lambda) that accepts an vector and returns a vector
#'
#' @return a ggplot
#' @export
#'
#' @examples
plot_regression = function(boots, facet = NULL, stat_label="Odds Ratio", report_fit = FALSE, limit = c(NA,NA), p.component=FALSE, label_fn = ~ .x) {
  
  facet = tryCatch(rlang::ensym(facet), error = function(e) NULL)
  facets = list(facet)
  is_facetted = is.null(facet)
  
  label_fn = purrr::as_mapper(getOption("tableone.labeller",label_fn))
  
  predictorVars = .parse_unique(boots$impute[[1]], boots$form, .side="rhs")
  keys = format_summary_rows(boots$impute[[1]], predictorVars)
  
  out1 = boots %>% combine_boots(predictorVars)
  
  
  out2 = out1 %>% 
    dplyr::group_by(group) %>%
    # striped groups
    dplyr::mutate(g = dplyr::cur_group_id() %% 2) %>%
    dplyr::group_by(group,subgroup) %>% dplyr::mutate(
      # height
      y=dplyr::cur_group_id(),
      # coefficient 
      x=exp(unname(beta.median)), 
      xmin=exp(unname(beta.lower)), 
      xmax=exp(unname(beta.upper))
    ) %>% dplyr::ungroup() %>% dplyr::mutate(
      y = max(y)-y,
    )

  statistic_label = stat_label
  
  groups = out2 %>% dplyr::select(!!!facets,group,g,y,x,xmin,xmax,statistic,p.value,p.component,type) %>% dplyr::mutate(
    y=y+0.1, 
    label=as.character(group), 
    label_x=0,
    x=ifelse(type=="continuous", x, NA_real_),
    xmin=ifelse(type=="continuous", xmin, NA_real_),
    xmax=ifelse(type=="continuous", xmax, NA_real_),
  ) %>% dplyr::group_by(!!!facets,group) %>% dplyr::filter(y==max(y)) %>% dplyr::ungroup()
  subgroups = out2 %>% dplyr::select(!!!facets,subgroup,g,y,x,xmin,xmax,statistic,p.value,p.component) %>% dplyr::filter(!is.na(subgroup)) %>% dplyr::mutate(label=as.character(subgroup), label_x=1)
  
  longer = dplyr::bind_rows(groups, subgroups) %>% dplyr::mutate(y=as.factor(dplyr::dense_rank(y))) %>%
    dplyr::mutate(
      p.flag = dplyr::case_when(
        p.component %>% stringr::str_starts("<") ~ "\u2020\u2020\u2020",
        suppressWarnings(as.numeric(p.component)) < 0.01 ~ "\u2020\u2020",
        suppressWarnings(as.numeric(p.component)) < 0.05 ~ "\u2020",
        TRUE ~ ""
      ),
      .facet = !!facet
    )
  
  lim_breaks = tryCatch(2^(seq(floor(log(limit[1],2)), ceiling(log(limit[2],2)), 1)), error = function(e) NULL)
  
  p1 = ggplot2::ggplot(longer, ggplot2::aes(y=y, x=x,xmin=xmin,xmax=xmax,colour=.facet))+
    ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g),x=1),width=Inf,colour=NA)+
    ggplot2::geom_point(position=ggstance::position_dodgev(height=0.5,preserve = "total"),size=0.4)+
    ggstance::geom_errorbarh(position=ggstance::position_dodgev(height=0.5,preserve = "total"),height=0.4)+ 
    (if(!is.null(lim_breaks)) ggplot2::scale_x_log10(breaks = lim_breaks) else ggplot2::scale_x_log10()) +
    ggplot2::coord_cartesian(xlim=limit)+
    ggplot2::geom_vline(xintercept = 1)+
    ggplot2::geom_text(data = longer %>% dplyr::select(y,!!!facets,subgroup,label,statistic) %>% dplyr::distinct(), mapping=ggplot2::aes(y=y,x=1.05,label=ifelse(!is.na(subgroup) & statistic=="ref","ref",NA)),inherit.aes = FALSE,size=.gg_label_size(5),hjust=0,colour="black")+
    ggplot2::geom_point(data = longer %>% dplyr::select(y,!!!facets,subgroup,label,statistic) %>% dplyr::filter(!is.na(subgroup) & statistic=="ref") %>% dplyr::distinct(), mapping=ggplot2::aes(y=y,x=1),inherit.aes = FALSE,colour="black")+
    .gg_hide_Y_axis()+
    ggplot2::xlab(statistic_label)+
    ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")+
    ggplot2::scale_color_brewer(palette = "Dark2")+
    ggplot2::facet_wrap(facets, nrow=1)+
    .gg_hide_legend()+
    .gg_set_X_angle(0)+
    if(p.component) {
      list(
        ggplot2::geom_text(data = longer %>% dplyr::filter(!is.na(p.component)), mapping=ggplot2::aes(y=y,x=0,label=p.flag),inherit.aes = FALSE,size=.gg_label_size(5),hjust=-0.1,colour="black"),
        ggplot2::labs(caption="\u2020\u2020\u2020 indicates p-value < 0.001, \u2020\u2020 p-value < 0.01 and \u2020 p-value < 0.05.")
      )
    } else {
      list()
    }
  
  if (longer %>% dplyr::filter(label==as.character(group) & !is.na(p.value)) %>% nrow() > 0) {
    p1 = p1+ggplot2::geom_text(data = longer %>% dplyr::filter(label==as.character(group) & !is.na(p.value)) %>% dplyr::mutate(label=paste0("(p: ",p.value,") ")), mapping=ggplot2::aes(y=y,x=Inf,label=label),inherit.aes = FALSE,size=.gg_label_size(5),hjust=1,colour="black")
  }
  
  p2 = ggplot2::ggplot(longer %>% dplyr::select(y,g,label_x,label) %>% dplyr::distinct(),ggplot2::aes(y=y))+
    ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g),x=1),width=Inf,colour=NA)+
    ggplot2::geom_text(mapping=ggplot2::aes(x=label_x, hjust=label_x, label=label),size=.gg_label_size(6))+
    ggplot2::coord_cartesian(xlim=c(0,1))+
    ggplot2::theme_void()+ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")
  
  
  if (!isFALSE(report_fit)) {
    perf = boots %>% .pool_performance(statistics = report_fit)
    perf2 = perf %>% dplyr::group_by(statistic) %>% dplyr::mutate(y=dplyr::cur_group_id(),g = dplyr::cur_group_id() %% 2) %>% dplyr::ungroup() %>% dplyr::mutate(y=max(y)-y)
    
    p3 = ggplot2::ggplot(perf2 %>% dplyr::select(statistic,y,g) %>% dplyr::distinct(), ggplot2::aes(y=y,label=statistic,x=1))+
      ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g)),width=Inf,colour=NA)+
      ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")+
      ggplot2::geom_text(size=.gg_label_size(6), hjust=1)+ggplot2::theme_void()+
      ggplot2::coord_cartesian(xlim = c(0,1))
    
    p4 = ggplot2::ggplot(perf2, ggplot2::aes(y=y,label=value,x=0))+
      ggplot2::geom_tile(mapping=ggplot2::aes(fill=as.character(g),x=0),width=Inf,colour=NA)+
      ggplot2::geom_text(size=.gg_label_size(6))+
      ggplot2::scale_fill_manual(values = c("0"="grey90","1"="white"), guide="none")+
      ggplot2::facet_wrap(facets,nrow=1)+.gg_hide_X_axis()+.gg_hide_Y_axis()+
      #ggplot2::theme_void()
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),axis.ticks.y = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), strip.background = ggplot2::element_blank())
    
    height_ratio= c(length(levels(longer$y)),length(report_fit))
    
    p = p2+p1+p3+p4+patchwork::plot_layout(ncol=2,nrow=2,widths = c(2.5,10),heights = height_ratio)
    
  } else {
    
    p = p2+p1+patchwork::plot_layout(ncol=2,nrow=1,widths = c(2.5,10))
    
  }
  
  p
}