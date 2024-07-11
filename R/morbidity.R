morbidity <- function(type,
                      gender = NULL,
                      age = NULL,
                      isch.heart = NULL,
                      cerebro.vasc = NULL,
                      heart.fail = NULL,
                      periph.vasc = NULL,
                      diabetes = NULL,
                      cancer = NULL,
                      cancer.hist = NULL,
                      schizo = NULL,
                      depression = NULL,
                      subst.abuse = NULL,
                      dementia = NULL,
                      parkinson = NULL,
                      mult.sclero = NULL,
                      epilespy = NULL,
                      chornic.resp = NULL,
                      infl.bowel = NULL,
                      rheum.arthr = NULL,
                      HIV = NULL,
                      es.renal = NULL,
                      liver.pancr = NULL) {

  type <- match.arg(type, choices = c("MRMI", "ERMI"))

  if (any(is.null(c(gender, age, isch.heart, cerebro.vasc, heart.fail,
                    periph.vasc, diabetes, cancer, cancer.hist, schizo,
                    depression, subst.abuse, dementia, parkinson,
                    mult.sclero, epilespy, chornic.resp, infl.bowel,
                    rheum.arthr, HIV, es.renal, liver.pancr))))

}
