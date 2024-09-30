#' X-13 Tables Available
#'
#' Generates a list of X-13 tables that can be extracted with the \code{seasonal} package.
#'
#' Version 2.4, 05/24/2023
#'
#' @param this_table_type vector of character strings listing types of X-13 tables to output. 
#'        Default is \code{'all'}, other choices are \code{'diagnostics'}, \code{'matrices'}, 
#'        \code{'spectrum'}, \code{'timeseries'}.
#' @return A list of arrays with table names and abbreviations from X-13ARIMA-SEATS in 
#'         several different elements specified by the user: diagnostics, matrices, spectrum, 
#'         timeseries
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' x13_tables_all <- gen_x13_table_list()
#' @export
gen_x13_table_list <- function(this_table_type = "all") {
    # Author: Brian C. Monsell (OEUS) Version 2.4, 05/24/2023
    
    # Initialize list of available X-13 tables
    x13_tables <- list()
    
    n_type <- length(this_table_type)
    
    for (i in 1:n_type) {
        if (this_table_type[i] == "all" | this_table_type[i] == "diagnostics") {
            x13_tables$diagnostics <- c("check.acf", "acf", "check.acfsquared", "ac2", "check.pacf", "pcf", "estimate.iterations", 
                "itr", "estimate.roots", "rts", "history.chngestimates", "che", "history.chngrevisions", "chr", 
                "history.fcsterrors", "fce", "history.fcsthistory", "fch", "history.indsaestimates", "iae", "history.indsarevisions", 
                "iar", "history.lkhdhistory", "lkh", "history.outlierhistory", "rot", "history.saestimates", 
                "sae", "history.sarevisions", "sar", "history.seatsmdlhistory", "smh", "history.sfestimates", 
                "sfe", "history.sfilterhistory", "sfh", "history.sfrevisions", "sfr", "history.trendchngestimates", 
                "tce", "history.trendchngrevisions", "tcr", "history.trendestimates", "tre", "history.trendrevisions", 
                "trr", "identify.acf", "iac", "identify.pacf", "ipc", "outlier.finaltests", "fts", "outlier.iterations", 
                "oit", "slidingspans.chngspans", "chs", "slidingspans.indchngspans", "cis", "slidingspans.indsaspans", 
                "ais", "slidingspans.indsfspans", "sis", "slidingspans.indychngspans", "yis", "slidingspans.sfspans", 
                "sfs", "slidingspans.tdspans", "tds", "slidingspans.ychngspans", "ycs", "x11.yrtotals", "e4", 
                "x11regression.x11reg", "c15", "x11regression.x11regb", "b15", "x11regression.outlieriter", "xoi")
        }
        
        if (this_table_type[i] == "all" | this_table_type[i] == "forecasts") {
            x13_tables$forecasts <- c("forecast.backcasts", "bct", "forecast.forecasts", "fct", "forecast.transformed", 
                "ftr", "forecast.transformedbcst", "btr", "forecast.variances", "fvr", "seats.seasonaladjfcstdecomp", 
                "afd", "seats.seasonalfcstdecomp", "sfd", "seats.seriesfcstdecomp", "ofd", "seats.transitoryfcstdecomp", 
                "yfd", "seats.trendfcstdecomp", "tfd")
        }
        
        if (this_table_type[i] == "all" | this_table_type[i] == "matrices") {
            x13_tables$matrices <- c("estimate.armacmatrix", "acm", "estimate.regcmatrix", "rcm", "regression.regressionmatrix", 
                "rmx", "x11regression.xregressioncmatrix", "xrc", "x11regression.xregressionmatrix", "xrm")
        }
        
        if (this_table_type[i] == "all" | this_table_type[i] == "spectrum") {
            x13_tables$spectrum <- c("spectrum.speccomposite", "is0", "spectrum.specindirr", "is2", "spectrum.specindsa", 
                "is1", "spectrum.specirr", "sp2", "spectrum.specorig", "sp0", "spectrum.specresidual", "spr", 
                "spectrum.specsa", "sp1", "spectrum.specseatsextresiduals", "ser", "spectrum.specseatsirr", "s2s", 
                "spectrum.specseatssa", "s1s")
        }
        
        if (this_table_type[i] == "all" | this_table_type[i] == "timeseries") {
            x13_tables$timeseries <- c("estimate.regressioneffects", "ref", "estimate.residuals", "rsd", "force.forcefactor", 
                "ffc", "force.revsachanges", "e6a", "force.rndsachanges", "e6r", "force.saround", "rnd", "force.seasadjtot", 
                "saa", "regression.aoutlier", "ao", "regression.holiday", "hol", "regression.levelshift", "ls", 
                "regression.outlier", "otl", "regression.regseasonal", "a10", "regression.seasonaloutlier", "so", 
                "regression.temporarychange", "tc", "regression.tradingday", "td", "regression.transitory", "a13", 
                "regression.userdef", "usr", "seats.adjustfac", "s16", "seats.adjustmentratio", "s18", "seats.cycle", 
                "cyc", "seats.diffseasonaladj", "dsa", "seats.difftrend", "dtr", "seats.irregular", "s13", "seats.longtermtrend", 
                "ltt", "seats.seasadjconst", "sec", "seats.seasonal", "s10", "seats.seasonaladj", "s11", "seats.seasonalsum", 
                "ssm", "seats.totaladjustment", "sta", "seats.transitory", "s14", "seats.trend", "s12", "seats.trendconst", 
                "stc", "series.adjoriginal", "b1", "series.calendaradjorig", "a18", "series.outlieradjorig", 
                "a19", "series.seriesmvadj", "mv", "series.span", "a1", "transform.permprior", "a2p", "transform.permprioradjusted", 
                "a3p", "transform.permprioradjustedptd", "a4p", "transform.prior", "a2", "transform.prioradjusted", 
                "a3", "transform.prioradjustedptd", "a4d", "transform.seriesconstant", "a1c", "transform.tempprior", 
                "a2t", "transform.transformed", "trn", "x11.adjoriginalc", "c1", "x11.adjoriginald", "d1", "x11.adjustdiff", 
                "fad", "x11.adjustfac", "d16", "x11.adjustmentratio", "e18", "x11.biasfactor", "bcf", "x11.calendar", 
                "d18", "x11.calendaradjchanges", "e8", "x11.combholiday", "chl", "x11.extreme", "c20", "x11.extremeb", 
                "b20", "x11.irregular", "d13", "x11.irregularadjao", "iao", "x11.irregularb", "b13", "x11.irregularc", 
                "c13", "x11.irrwt", "c17", "x11.irrwtb", "b17", "x11.mcdmovavg", "f1", "x11.modirregular", "e3", 
                "x11.modoriginal", "e1", "x11.modseasadj", "e2", "x11.modsic4", "c4", "x11.modsid4", "d4", "x11.origchanges", 
                "e5", "x11.replacsi", "d9", "x11.replacsic9", "c9", "x11.robustsa", "e11", "x11.sachanges", "e6", 
                "x11.seasadj", "d11", "x11.seasadjb11", "b11", "x11.seasadjb6", "b6", "x11.seasadjc11", "c11", 
                "x11.seasadjc6", "c6", "x11.seasadjconst", "sac", "x11.seasadjd6", "d6", "x11.seasonal", "d10", 
                "x11.seasonaladjregsea", "ars", "x11.seasonalb10", "b10", "x11.seasonalb5", "b5", "x11.seasonalc10", 
                "c10", "x11.seasonalc5", "c5", "x11.seasonald5", "d5", "x11.seasonaldiff", "fsd", "x11.sib3", 
                "b3", "x11.sib8", "b8", "x11.tdadjorig", "c19", "x11.tdadjorigb", "b19", "x11.totaladjustment", 
                "tad", "x11.trend", "d12", "x11.trendadjls", "tal", "x11.trendb2", "b2", "x11.trendb7", "b7", 
                "x11.trendc2", "c2", "x11.trendc7", "c7", "x11.trendchanges", "e7", "x11.trendconst", "tac", 
                "x11.trendd2", "d2", "x11.trendd7", "d7", "x11.unmodsi", "d8", "x11.unmodsiox", "d8b", "x11regression.calendar", 
                "xca", "x11regression.calendarb", "bxc", "x11regression.combcalendar", "xcc", "x11regression.combcalendarb", 
                "bcc", "x11regression.combtradingday", "c18", "x11regression.combtradingdayb", "b18", "x11regression.extremeval", 
                "c14", "x11regression.extremevalb", "b14", "x11regression.holiday", "xhl", "x11regression.holidayb", 
                "bxh", "x11regression.priortd", "a4", "x11regression.tradingday", "c16", "x11regression.tradingdayb", 
                "b16")
        }
    }
    return(x13_tables)
    
}
