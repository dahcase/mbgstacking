library(h2o)

h2o.init()

#requires running stuff to create stk
blerf = stk$data
blerf[, offset := log(N)]

blerg = as.h2o(blerf)

mod = h2o.glm(x =  c('lights_new', 'access', 'evi', 'ldi_pc', 'health_system_access_capped'), y = 'fever',training_frame = blerg, offset_column = 'offset', family = 'poisson')

preds = predict(mod, blerg, type = 'response')

ln = 2.55660732
ac = -1.0097876
evi = 1.005273
ldi = -1.135976
hsa = -1.271793

res = -1.666690 + 0.031515 * ln + 0.042567 * ac + 0.111958 * evi + 0.335792 * ldi + -0.168306 * hsa
