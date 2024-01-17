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

tt_crude_foi[] <- user()
dim(tt_crude_foi) <- user()

crude_foi[] <- user()
dim(crude_foi) <- length(tt_crude_foi)

t_crude_foi <- interpolate(tt_crude_foi, crude_foi, "constant")

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

#transistions

total_pop[1:n_maternal] <- R[i] + S[i] + V[i] + M[i] + VD[i]
total_pop[n_maternal:n_age] <- R[i] + S[i] + V[i] + VD[i]
dim(total_pop) <- n_age

total_child_bearing[] <- total_pop[i] * child_bearing[i]
dim(total_child_bearing) <- n_age

susceptible_pop[] <- S[i] + VD[i]
dim(susceptible_pop) <- n_age

susceptible_child_bearing[] <- (susceptible_pop[i]) * child_bearing[i]
dim(susceptible_child_bearing) <- n_age

births_total <- (sum(total_pop[])) * t_crude_birth_rate

births_S <- births_total * (sum(susceptible_child_bearing[]) / sum(total_child_bearing[]))

births_M <- births_total - births_S

natural_waning[] <- waning * R[i]
dim(natural_waning) <- n_age

vaccine_waning[] <- waning * V[i]
dim(vaccine_waning) <- n_age

vaccine_partial_waning[] <- waning * VD[i]
dim(vaccine_partial_waning) <- n_age

#must adjust crude foi for the fact that some people are immune

t_adjusted_foi <- t_crude_foi * sum(total_pop[]) / sum(susceptible_pop[])

infections[] <- t_adjusted_foi * S[i]
dim(infections) <- n_age

infections_VD[] <- t_adjusted_foi * VD[i]
dim(infections_VD) <- n_age

#adjust death rate so that it matches crude rate
weighted_totals[] <- prop_death[i] * total_pop[i]
dim(weighted_totals) <- n_age

t_death_rate[] <- t_crude_death_rate * prop_death[i] * sum(total_pop[]) / sum(weighted_totals[])
dim(t_death_rate) <- n_age

deaths_S[] <- t_death_rate[i] * S[i]
dim(deaths_S) <- n_age

deaths_R[] <- t_death_rate[i] * R[i]
dim(deaths_R) <- n_age

deaths_V[] <- t_death_rate[i] * V[i]
dim(deaths_V) <- n_age

deaths_VD[] <- t_death_rate[i] * VD[i]
dim(deaths_VD) <- n_age

deaths_M[] <- t_death_rate[i] * M[i]
dim(deaths_M) <- n_maternal

ageing_S[] <- age_rate[i] * S[i]
dim(ageing_S) <- n_age

ageing_R[] <- age_rate[i] * R[i]
dim(ageing_R) <- n_age

ageing_V[] <- age_rate[i] * V[i]
dim(ageing_V) <- n_age

ageing_VD[] <- age_rate[i] * VD[i]
dim(ageing_VD) <- n_age

ageing_M[] <- age_rate[i] * M[i]
dim(ageing_M) <- n_maternal

#assume repeat vaccination risk is independent of vaccination status

ageing_S_vaccinated[] <- t_vaccination_coverage[i] * ageing_S[i]
dim(ageing_S_vaccinated) <- n_age

ageing_S_partial_vaccinated[] <- t_vaccination_partial_coverage[i] * ageing_S[i]
dim(ageing_S_partial_vaccinated) <- n_age

ageing_S_not_vaccinated[] <- ageing_S[i] - ageing_S_vaccinated[i] - ageing_S_partial_vaccinated[i]
dim(ageing_S_not_vaccinated) <- n_age

ageing_R_vaccinated[] <- t_vaccination_coverage[i] * ageing_R[i]
dim(ageing_R_vaccinated) <- n_age

ageing_R_not_vaccinated[] <- ageing_R[i] - ageing_R_vaccinated[i]
dim(ageing_R_not_vaccinated) <- n_age

ageing_M_vaccinated[] <- t_vaccination_coverage[i] * ageing_M[i]
dim(ageing_M_vaccinated) <- n_maternal

ageing_M_partial_vaccinated[] <- t_vaccination_partial_coverage[i] * ageing_M[i]
dim(ageing_M_partial_vaccinated) <- n_maternal #bit of an issue here, vaccination makes immunity weaker

ageing_M_not_vaccinated[] <- ageing_M[i] - ageing_M_vaccinated[i] - ageing_M_partial_vaccinated[i]
dim(ageing_M_not_vaccinated) <- n_maternal

ageing_VD_vaccinated[] <-  t_vaccination_coverage[i] * ageing_VD[i]
dim(ageing_VD_vaccinated) <- n_age

ageing_VD_not_vaccinated[] <- ageing_VD[i] - ageing_VD_vaccinated[i]
dim(ageing_VD_not_vaccinated) <- n_age

age_group_loses_maternal <- n_maternal + 1
age_group_no_maternal <- age_group_loses_maternal + 1

#derivatives
dim(S) <- n_age
initial(S[]) <- S_0[i]
deriv(S[1]) <- births_S + natural_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[2:n_maternal]) <- ageing_S_not_vaccinated[i-1] + natural_waning[i] + vaccine_waning[i] + vaccine_partial_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[age_group_loses_maternal]) <- ageing_M_not_vaccinated[i-1] + ageing_S_not_vaccinated[i-1] + natural_waning[i] + vaccine_waning[i] + vaccine_partial_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[age_group_no_maternal:n_age]) <- ageing_S_not_vaccinated[i-1] + natural_waning[i] + vaccine_waning[i] + vaccine_partial_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]

dim(R) <- n_age
initial(R[]) <- R_0[i]
deriv(R[1]) <- infections[i] - ageing_R[i] - deaths_R[i] - natural_waning[i]
deriv(R[2:n_age]) <- infections[i] + infections_VD[i] + ageing_R_not_vaccinated[i-1] - ageing_R[i] - deaths_R[i] - natural_waning[i]

dim(V) <- n_age
initial(V[]) <- 0
deriv(V[1]) <- 0
deriv(V[2:age_group_loses_maternal]) <- ageing_M_vaccinated[i-1] + ageing_S_vaccinated[i-1] + ageing_R_vaccinated[i-1] + ageing_VD_vaccinated[i-1] + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]
deriv(V[age_group_no_maternal:n_age]) <- ageing_S_vaccinated[i-1] + ageing_R_vaccinated[i-1] + ageing_VD_vaccinated[i-1] + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]

dim(VD) <- n_age
initial(VD[]) <- 0
deriv(VD[1]) <- 0
deriv(VD[2:age_group_loses_maternal]) <- ageing_M_partial_vaccinated[i-1] + ageing_S_partial_vaccinated[i-1] + ageing_VD_not_vaccinated[i-1] - ageing_VD[i] - deaths_VD[i] - vaccine_partial_waning[i] - infections_VD[i]
deriv(VD[age_group_no_maternal:n_age]) <- ageing_S_partial_vaccinated[i-1] + ageing_VD_not_vaccinated[i-1] - ageing_VD[i] - deaths_VD[i] - vaccine_partial_waning[i] - infections_VD[i]

dim(M) <- n_maternal
initial(M[]) <- M_0[i]
deriv(M[1]) <- births_M - ageing_M[1] - deaths_M[1]
deriv(M[2:n_maternal]) <- ageing_M_not_vaccinated[1] - ageing_M[2] - deaths_M[2]