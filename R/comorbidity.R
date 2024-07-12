comorbidity <- function(age = NULL,
                      myocard.infarct = NULL,
                      heart.fail = NULL,
                      periph.vasc = NULL,
                      cerebr = NULL,
                      dementia = NULL,
                      chornic.pulm = NULL,
                      connect.tissue = NULL,
                      ulcer = NULL,
                      mild.liver = NULL,
                      diabetes = NULL,
                      hemiplegia = NULL,
                      renal = NULL,
                      diabetes.org.dmg = NULL,
                      tumor = NULL,
                      leukemia = NULL,
                      lymphoma = NULL,
                      mode.sev.liver = NULL,
                      metast.tumor = NULL,
                      aids = NULL) {
  

  
  if (!is.numeric(age)) stop("Age must be numeric (years)")
  
  if (is.null(age) | 
      is.null(myocard.infarct) | is.null(heart.fail) |
      is.null(periph.vasc) | is.null(cerebr) | 
      is.null(dementia) | is.null(chornic.pulm) |
      is.null(connect.tissue) | is.null(ulcer) | 
      is.null(mild.liver) | 
      is.null(hemiplegia) | is.null(renal) | 
      is.null(diabetes.org.dmg) | is.null(tumor) |
      is.null(leukemia) | is.null(lymphoma) | 
      is.null(mode.sev.liver) | is.null(metast.tumor) |
      is.null(aids)) {
    stop("All arguments are required")
  }
  
  if (!(all(myocard.infarct %in% c(0, 1, TRUE, FALSE)) & all(heart.fail %in% c(0, 1, TRUE, FALSE)) &
        all(periph.vasc %in% c(0, 1, TRUE, FALSE)) & all(cerebr %in% c(0, 1, TRUE, FALSE)) &
        all(diabetes %in% c(0, 1, TRUE, FALSE)) & 
        all(dementia %in% c(0, 1, TRUE, FALSE)) & all(chornic.pulm %in% c(0, 1, TRUE, FALSE)) &
        all(connect.tissue %in% c(0, 1, TRUE, FALSE)) & all(ulcer %in% c(0, 1, TRUE, FALSE)) &
        all(mild.liver %in% c(0, 1, TRUE, FALSE)) & 
        all(hemiplegia %in% c(0, 1, TRUE, FALSE)) & all(renal %in% c(0, 1, TRUE, FALSE)) &
        all(diabetes.org.dmg %in% c(0, 1, TRUE, FALSE)) & all(tumor %in% c(0, 1, TRUE, FALSE)) &
        all(leukemia %in% c(0, 1, TRUE, FALSE)) & all(lymphoma %in% c(0, 1, TRUE, FALSE)) &
        all(mode.sev.liver %in% c(0, 1, TRUE, FALSE)) & all(metast.tumor %in% c(0, 1, TRUE, FALSE)) &
        all(aids %in% c(0, 1, TRUE, FALSE)))) {
    stop("All arguments accept type must be c(0,1) or c(TRUE, FALSE)")
  }
  
  ind_charlson <- floor((age - 40)/10) +
    myocard.infarct * 1 + heart.fail * 1 + 
    periph.vasc * 1 + cerebr * 1 +
    diabetes * 1 + 
    dementia * 1 + chornic.pulm * 1 + 
    connect.tissue * 1 + ulcer * 1 + 
    mild.liver * 1 + 
    hemiplegia * 2 + renal * 2 +
    diabetes.org.dmg * 2 +  tumor * 2 + 
    leukemia * 2 + lymphoma * 2 + 
    mode.sev.liver * 3 + metast.tumor * 6  +
    aids * 6

  return(c(CCI = ind_charlson))
}



# comorbidity(
#   age = 75,
#   myocard.infarct = 0,
#   heart.fail = 0,
#   periph.vasc = 0,
#   cerebr = TRUE,
#   diabetes = 0,
#   dementia = 0,
#   chornic.pulm = 0,
#   connect.tissue = 0,
#   ulcer = 0,
#   mild.liver = 0,
#   hemiplegia = 0,
#   renal = TRUE,
#   diabetes.org.dmg = 0,
#   tumor = 0,
#   leukemia = 0,
#   lymphoma = 0,
#   mode.sev.liver = 0,
#   metast.tumor = 0,
#   aids = 0
# )
#
#
# comorbidity(gender = "M",
#           age = sample(50:90, 100, replace = TRUE),
#           isch.heart = sample(c(0,1), 100, replace = TRUE),
#           cerebro.vasc = sample(c(0,1), 100, replace = TRUE),
#           heart.fail = sample(c(0,1), 100, replace = TRUE),
#           periph.vasc = sample(c(0,1), 100, replace = TRUE),
#           diabetes = sample(c(0,1), 100, replace = TRUE),
#           cancer = sample(c(0,1), 100, replace = TRUE),
#           cancer.hist = sample(c(0,1), 100, replace = TRUE),
#           schizo = sample(c(0,1), 100, replace = TRUE),
#           depression = sample(c(0,1), 100, replace = TRUE),
#           subst.abuse = sample(c(0,1), 100, replace = TRUE),
#           dementia = sample(c(0,1), 100, replace = TRUE),
#           parkinson = sample(c(0,1), 100, replace = TRUE),
#           mult.sclero = sample(c(0,1), 100, replace = TRUE),
#           epilespy = sample(c(0,1), 100, replace = TRUE),
#           chornic.resp = sample(c(0,1), 100, replace = TRUE),
#           infl.bowel = sample(c(0,1), 100, replace = TRUE),
#           rheum.arthr = sample(c(0,1), 100, replace = TRUE),
#           HIV = sample(c(0,1), 100, replace = TRUE),
#           es.renal = sample(c(0,1), 100, replace = TRUE),
#           liver.pancr = sample(c(0,1), 100, replace = TRUE))
