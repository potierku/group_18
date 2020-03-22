library(kernlab)
data("spam")
tibble::as.tibble(spam)

is.factor(spam$type)
levels(spam$type)

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)

# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

################exercise 1, part 1, q1#######################
#see docs
##############exercise 1, part 1, q2########################
set.seed(3)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]

###############exercise 1, part 2########################
make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}
#############caps########
spam_tst_pred = ifelse(predict(fit_caps, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_caps = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
accuracy_caps <-length(which(spam_tst_pred==spam_tst$type))/length(spam_tst$type)

############selected##############
spam_tst_pred = ifelse(predict(fit_selected, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_selected = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
accuracy_selected <-length(which(spam_tst_pred==spam_tst$type))/length(spam_tst$type)

###########additive############
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_additive = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
accuracy_additive <-length(which(spam_tst_pred==spam_tst$type))/length(spam_tst$type)

###########over##############
spam_tst_pred = ifelse(predict(fit_over, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
(conf_mat_over = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
accuracy_over <-length(which(spam_tst_pred==spam_tst$type))/length(spam_tst$type)

############table############
table(spam_tst$type) / nrow(spam_tst)

