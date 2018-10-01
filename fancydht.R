# do some fancy density estimation

# this is probably easier with plyr stuff?

library(plyr)

fancydht <- function(model, flatfile, formula, geo=FALSE, convert.units=1){
  # model     - an output from either Distance::ds() or mrds::ddf()
  # formula   - a formula telling fancydht() how to calculate abundance ests
  ## flatfile  - data in flatfile format
  ## region/obs/sample tables
  # geo       - is stratification geographic
  #             (TRUE means segments don't get duplciated)


  # slice and dice that object
  if(all(class(model)=="dsmodel")){
    ddf <- model$ddf
    dht <- model$dht
  }else{
    ddf <- model
    stop("mrds models not implemented yet")
  }


  # extract the flatfile data
## TODO
  #flatfile <- ddf$data
  ### truncate
  ##flatfile <- flatfile[flatfile$distance<=ddf$meta.data$width, ]
  ##flatfile <- flatfile[flatfile$distance>=ddf$meta.data$left, ]

  ## checks
  # is the term in the data.frame?
  formula_cols <- attr(terms(formula), "term.labels")
  if(!all(formula_cols %in% colnames(flatfile))){
    stop("Stratification variables not in data")
  }
  # only do one strat variable for now
  if(length(formula_cols) > 1){
    stop("Only one stratification variable is supported at the moment")
  }
  # data must have 1 Region.Label for now
  if(length(unique(flatfile$Region.Label)) > 1){
    stop("Only one Region.Label allowed in data")
  }
  if(length(unique(flatfile$Area)) > 1){
    stop("Only one Area value allowed in data")
  }




  # juggle the data
  if(geo){
    stop("geographic stratification not implemented yet, even though it's easier")
  }else{

    # get unique Sample.Labels
    samplelabs <- unique(flatfile$Sample.Label)
    # get unique covariate values
    unique_covars <- unique(flatfile[, formula_cols])

    # make a grid of new values
    dummy_dat <- expand.grid(Sample.Label = samplelabs,
                             Region.Label = unique_covars)
    dummy_dat$Area <- unique(flatfile$Area)
    # grab areas
    dummy_dat <- merge(dummy_dat,
                       unique(flatfile[,c("Sample.Label","Effort")]))
    # fill in blank distances
    dummy_dat$distance <- NA
    dummy_dat$object <- NA
    # make the stratification variable the region label
    flatfile$Region.Label <- flatfile[[formula_cols]]
    # strip out other columns
    flatfile <- flatfile[, c("Region.Label", "Area", "Sample.Label", "Effort",
                             "distance", "object")]
    # smoosh
    flatfile <- rbind(flatfile, dummy_dat)
    # get rid of duplicates
    ff <- ddply(flatfile, .(Sample.Label, Region.Label), function(x){
                xx <- x[!is.na(x$distance),]
                if(nrow(xx)>0) return(xx)
                return(unique(x[is.na(x$distance),]))})
#ff<-flatfile
  }

  ## unflatten the file
  anti_ikea <- unflatten(ff)
  sample.table <- anti_ikea$sample.table
  region.table <- anti_ikea$region.table
  obs.table <- anti_ikea$obs.table

  # estimate abundance
  dhat <- dht(ddf, region.table, sample.table, obs.table,
              options=list(convert.units=convert.units))

  return(dhat)

}
