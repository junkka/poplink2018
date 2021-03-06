#' @name eh_data
#' @title Event history data
#' @description Event history data for POPLINK couples
#' @docType data
#' @usage data(eh_data)
#' @format A \code{tbl_df} with 309,266 observations and 60 variables
#' \describe{
#'   \item{mid}{women id}
#'   \item{pid}{partner id}
#'   \item{date}{event date}
#'   \item{type}{event type}
#'   \item{marr_date}{Marriage date}
#'   \item{end_date}{maximum date for couple}
#'   \item{m_nykt_ind}{Indicator for if women was ever a temperance member}
#'   \item{p_nykt_ind}{Indicator for if partner was ever a temperance member}
#'   \item{fack_ind}{Indicator for if partner was ever a union member}
#'   \item{offset}{event offset}
#'   \item{parish}{parish of residance at event}
#'   \item{bdate}{women birth date}
#'   \item{cid}{ID of previously born child}
#'   \item{nr}{event order number}
#'   \item{mult_id}{ID of multiple birth}
#'   \item{multiple}{Multiple status of cid}
#'   \item{gender}{gender of cid}
#'   \item{children}{number of children alive}
#'   \item{boys}{number of boys alive}
#'   \item{girls}{number of girls alive}
#'   \item{prev_birth_id}{cid}
#'   \item{parity}{current birth parity}
#'   \item{occu}{partner occupational code}
#'   \item{hisco}{occupation in hisco code}
#'   \item{socpo_label}{SOCPO class label}
#'   \item{hisclass}{hisclass 5 label}
#'   \item{ses}{Social class from HISCLASS}
#'   \item{ses_socpo}{aggregation of socpo}
#'   \item{m_nykt}{women previously joined a temperance association}
#'   \item{m_nykt_ar}{year of women joining a temperance association}
#'   \item{p_nykt}{partner previously joined a temperance association}
#'   \item{p_nykt_ar}{year of partner joining a temperance association}
#'   \item{p_fack}{partner previously joined a union}
#'   \item{p_fack_ar}{year of partner joining a union}
#'   \item{nykt}{women or partner previously joined a temperance association}
#'   \item{fack}{women or partner previously joined a union}
#'   \item{nykt_ar}{year of nykt}
#'   \item{fack_ar}{year of fack}
#'   \item{place}{current place of residance}
#'   \item{con_ind}{Conseption indicator}
#'   \item{x}{longitudinal coordinat of place}
#'   \item{y}{latitudal coordinat of place}
#'   \item{mindate}{earlies observed date for couple}
#'   \item{voro}{sub-regional residance}
#'   \item{fackf_size}{size of union in voro}
#'   \item{nykt_size}{size of temperance associations in voro}
#'   \item{fackf_nei}{size of union in neighbouring voro}
#'   \item{nykt_nei}{size of temperance in neighbouring voro}
#'   \item{m_mig}{migrant status of women}
#'   \item{p_mig}{migrant status of partner}
#'   \item{year}{year of event}
#'   \item{mig_prop}{proportion migrants in voro}
#'   \item{density}{population density in voro}
#'   \item{population}{population in voro}
#' }
#' @source Based on parish borders from Riksarkivet.
NULL