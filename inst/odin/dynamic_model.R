#inputs
n_age <- user()
n_maternal <- user()

child_bearing[] <- user()
dim(child_bearing) <- n_age

age_rate[] <- user()
dim(age_rate) <- n_age

tt_crude_death_rate[] <- user()
dim(tt_crude_death_rate) <- user()

crude_death_rate[] <- user()
dim(crude_death_rate) <- length(tt_crude_death_rate)

t_crude_death_rate <- interpolate(tt_crude_death_rate, crude_death_rate, "constant")

prop_death[] <- user()
dim(prop_death) <- n_age

tt_crude_birth_rate[] <- user()
dim(tt_crude_birth_rate) <- user()

crude_birth_rate[] <- user()
dim(crude_birth_rate) <- length(tt_crude_birth_rate)

t_crude_birth_rate <- interpolate(tt_crude_birth_rate, crude_birth_rate, "constant")

waning <- user()

pre_infectious_transition_rate <- user()

infectious_transition_rate <- user()

tt_R0[] <- user()
dim(tt_R0) <- user()

R0[] <- user()
dim(R0) <- length(tt_R0)

t_R0 <- interpolate(tt_R0, R0, "constant")

beta <- t_R0 * infectious_transition_rate

tt_vaccination_coverage[] <- user()
dim(tt_vaccination_coverage) <- user()

vaccination_coverage[,] <- user()
dim(vaccination_coverage) <- c(length(tt_vaccination_coverage), n_age)

t_vaccination_coverage[] <- interpolate(tt_vaccination_coverage, vaccination_coverage, "constant")
dim(t_vaccination_coverage) <- n_age

vaccination_partial_coverage[,] <- user()
dim(vaccination_partial_coverage) <- c(length(tt_vaccination_coverage), n_age)

t_vaccination_partial_coverage[] <- interpolate(tt_vaccination_coverage, vaccination_partial_coverage, "constant")
dim(t_vaccination_partial_coverage) <- n_age

M_0[] <- user()
dim(M_0) <- n_maternal

S_0[] <- user()
dim(S_0) <- n_age

R_0[] <- user()
dim(R_0) <- n_age

I_0[] <- user()
dim(I_0) <- n_age

#transistions
total_pop[1:n_maternal] <- R[i] + S[i] + V[i] + M[i] + VD[i] + I[i] + E[i]
total_pop[(n_maternal + 1):n_age] <- R[i] + S[i] + V[i] + VD[i] + I[i] + E[i]
dim(total_pop) <- n_age

total_child_bearing[] <- total_pop[i] * child_bearing[i]
dim(total_child_bearing) <- n_age

susceptible_pop[] <- S[i] + VD[i] + I[i] + E[i]
dim(susceptible_pop) <- n_age

susceptible_child_bearing[] <- (susceptible_pop[i]) * child_bearing[i]
dim(susceptible_child_bearing) <- n_age

births_total <- (sum(total_pop[])) * t_crude_birth_rate

births_S <- births_total * (sum(susceptible_child_bearing[]) / sum(total_child_bearing[]))

births_M <- births_total - births_S

#adjust death rate so that it matches crude rate
weighted_totals[] <- prop_death[i] * total_pop[i]
dim(weighted_totals) <- n_age

t_death_rate[] <- min(t_crude_death_rate * prop_death[i] * sum(total_pop[]) / sum(weighted_totals[]), 1)
dim(t_death_rate) <- n_age

births_deaths_S[1] <- births_S - t_death_rate[i] * S[i]
births_deaths_S[2:n_age] <- -t_death_rate[i] * S[i]
dim(births_deaths_S) <- n_age

births_deaths_E[] <- -t_death_rate[i] * E[i]
dim(births_deaths_E) <- n_age

births_deaths_I[] <- -t_death_rate[i] * I[i]
dim(births_deaths_I) <- n_age

births_deaths_R[] <- -t_death_rate[i] * R[i]
dim(births_deaths_R) <- n_age

births_deaths_V[] <- -t_death_rate[i] * V[i]
dim(births_deaths_V) <- n_age

births_deaths_VD[] <- -t_death_rate[i] * VD[i]
dim(births_deaths_VD) <- n_age

births_deaths_M[1] <- births_M - t_death_rate[i] * M[i]
births_deaths_M[2:n_maternal] <- - t_death_rate[i] * M[i]
dim(births_deaths_M) <- n_maternal

#waning
waning_R[] <- waning * R[i]
dim(waning_R) <- n_age

waning_V[] <- waning * V[i]
dim(waning_V) <- n_age

waning_VD[] <- waning * VD[i]
dim(waning_VD) <- n_age

loses_immunity[] <- waning_R[i] + waning_V[i] + waning_VD[i]
dim(loses_immunity) <- n_age

#calculate foi
foi <- beta * sum(I[]) #completely random mixing

infections_S[] <- foi * S[i]
dim(infections_S) <- n_age

infections_VD[] <- foi * VD[i]
dim(infections_VD) <- n_age

becomes_pre_infectious[] <- infections_S[i] + infections_VD[i]
dim(becomes_pre_infectious) <- n_age

becomes_infectious[] <- pre_infectious_transition_rate * E[i]
dim(becomes_infectious) <- n_age

gains_immunity[] <- infectious_transition_rate * I[i]
dim(gains_immunity) <- n_age

#ageing process
age_group_loses_maternal <- n_maternal + 1
age_group_no_maternal <- age_group_loses_maternal + 1
n_age_penultimate <- n_age - 1

#assume repeat vaccination risk is independent of vaccination status

##Susceptible

ages_up_S[] <- S[i] * age_rate[i]
dim(ages_up_S) <- n_age

ages_up_S_v[] <- t_vaccination_coverage[i] * ages_up_S[i]
dim(ages_up_S_v) <- n_age

ages_up_S_pv[] <- t_vaccination_partial_coverage[i] * ages_up_S[i]
dim(ages_up_S_pv) <- n_age

ages_up_S_nv[] <- ages_up_S[i] - ages_up_S_v[i] - ages_up_S_pv[i]
dim(ages_up_S_nv) <- n_age

ageing_S[1] <- -ages_up_S[i]
ageing_S[2:n_maternal] <- ages_up_S_nv[i-1] - ages_up_S[i]
ageing_S[age_group_loses_maternal] <- ages_up_S_nv[i-1] + ages_up_M_nv[i-1] - ages_up_S[i]
ageing_S[age_group_no_maternal:n_age_penultimate] <- ages_up_S_nv[i-1] - ages_up_S[i]
ageing_S[n_age] <- ages_up_S_nv[i-1]
dim(ageing_S) <- n_age

##Acquired Immunity

#pre infectious (E), no post-exposure prophylaxis

ages_up_E[] <- E[i] * age_rate[i]
dim(ages_up_E) <- n_age

ageing_E[1] <- -ages_up_E[i]
ageing_E[2:n_age_penultimate] <- ages_up_E[i-1] - ages_up_E[i]
ageing_E[n_age] <- ages_up_E[i-1]
dim(ageing_E) <- n_age

#infectious (I), no post-exposure prophylaxis

ages_up_I[] <- I[i] * age_rate[i]
dim(ages_up_I) <- n_age

ageing_I[1] <- -ages_up_I[i]
ageing_I[2:n_age_penultimate] <- ages_up_I[i-1] - ages_up_I[i]
ageing_I[n_age] <- ages_up_I[i-1]
dim(ageing_I) <- n_age

#actually immune

ages_up_R[] <- R[i] * age_rate[i]
dim(ages_up_R) <- n_age

ages_up_R_v[] <- t_vaccination_coverage[i] * ages_up_R[i]
dim(ages_up_R_v) <- n_age

ages_up_R_nv[] <- ages_up_R[i] - ages_up_R_v[i]
dim(ages_up_R_nv) <- n_age

ageing_R[1] <- -ages_up_R[i]
ageing_R[2:n_age_penultimate] <- ages_up_R_nv[i-1] - ages_up_R[i]
ageing_R[n_age] <- ages_up_R_nv[i-1]
dim(ageing_R) <- n_age

##Fully Protected

ages_up_V[] <- V[i] * age_rate[i]
dim(ages_up_V) <- n_age

ageing_V[1] <- -ages_up_V[i]
ageing_V[2:age_group_loses_maternal] <- ages_up_M_v[i-1] + ages_up_V[i-1] + ages_up_S_v[i-1] + ages_up_R_v[i-1] + ages_up_VD_v[i-1] - ages_up_V[i]
ageing_V[age_group_no_maternal:n_age_penultimate] <- ages_up_V[i-1] + ages_up_S_v[i-1] + ages_up_R_v[i-1] + ages_up_VD_v[i-1] - ages_up_V[i]
ageing_V[n_age] <- ages_up_V[i-1] + ages_up_S_v[i-1] + ages_up_R_v[i-1] + ages_up_VD_v[i-1]
dim(ageing_V) <- n_age

##Partially Protected

ages_up_VD[] <- VD[i] * age_rate[i]
dim(ages_up_VD) <- n_age

ages_up_VD_v[] <- t_vaccination_coverage[i] * ages_up_VD[i]
dim(ages_up_VD_v) <- n_age

ages_up_VD_nv[] <- ages_up_VD[i] - ages_up_VD_v[i]
dim(ages_up_VD_nv) <- n_age

ageing_VD[1] <- -ages_up_VD[i]
ageing_VD[2:n_age_penultimate] <- ages_up_VD_nv[i-1] + ages_up_S_pv[i-1] - ages_up_VD[i]
ageing_VD[n_age] <- ages_up_VD_nv[i-1] + ages_up_S_pv[i-1]
dim(ageing_VD) <- n_age

##Maternal
#Don't handle partial protection here

ages_up_M[] <- M[i] * age_rate[i]
dim(ages_up_M) <- n_maternal

ages_up_M_v[] <- t_vaccination_coverage[i] * ages_up_M[i]
dim(ages_up_M_v) <- n_maternal

ages_up_M_nv[] <- ages_up_M[i] - ages_up_M_v[i]
dim(ages_up_M_nv) <- n_maternal

ageing_M[1] <- -ages_up_M[i]
ageing_M[2:n_maternal] <- ages_up_M_nv[i-1] - ages_up_M[i]
dim(ageing_M) <- n_maternal


#derivatives
dim(S) <- n_age
initial(S[]) <- S_0[i]
deriv(S[]) <- births_deaths_S[i] + ageing_S[i] + loses_immunity[i] - infections_S[i]

dim(E) <- n_age
initial(E[]) <- 0
deriv(E[]) <- births_deaths_E[i] + ageing_E[i] + becomes_pre_infectious[i] - becomes_infectious[i]

dim(I) <- n_age
initial(I[]) <- I_0[i]
deriv(I[]) <- births_deaths_I[i] + ageing_I[i] + becomes_infectious[i] - gains_immunity[i]

dim(R) <- n_age
initial(R[]) <- R_0[i]
deriv(R[]) <- births_deaths_R[i] + ageing_R[i] + gains_immunity[i] - waning_R[i]

dim(V) <- n_age
initial(V[]) <- 0
deriv(V[]) <- births_deaths_V[i] + ageing_V[i] - waning_V[i]

dim(VD) <- n_age
initial(VD[]) <- 0
deriv(VD[]) <- births_deaths_VD[i] + ageing_VD[i] - waning_VD[i] - infections_VD[i]

dim(M) <- n_maternal
initial(M[]) <- M_0[i]
deriv(M[]) <- births_deaths_M[i] + ageing_M[i]