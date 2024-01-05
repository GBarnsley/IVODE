#inputs
n_age <- user()

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

maternal_waning <- user()

waning <- user()

tt_crude_foi[] <- user()
dim(tt_crude_foi) <- user()

crude_foi[] <- user()
dim(crude_foi) <- length(tt_crude_foi)

t_crude_foi <- interpolate(tt_crude_foi, crude_foi, "constant")

vaccine_efficacy <- user()

tt_vaccination_rate[] <- user()
dim(tt_vaccination_rate) <- user()

vaccination_rate[,] <- user()
dim(vaccination_rate) <- c(length(tt_vaccination_rate), n_age)

t_vaccination_rate[] <- interpolate(tt_vaccination_rate, vaccination_rate, "constant")
dim(t_vaccination_rate) <- n_age

M_0[] <- user()
dim(M_0) <- 2

S_0[] <- user()
dim(S_0) <- n_age

R_0[] <- user()
dim(R_0) <- n_age

#transistions

total_pop[1:2] <- R[i] + S[i] + V[i] + M[i]
total_pop[3:n_age] <- R[i] + S[i] + V[i]
dim(total_pop) <- n_age

total_child_bearing[] <- total_pop[i] * child_bearing[i]
dim(total_child_bearing) <- n_age

susceptible_child_bearing[] <- S[i] * child_bearing[i]
dim(susceptible_child_bearing) <- n_age

births_total <- (sum(total_pop[])) * t_crude_birth_rate

births_S <- births_total * (sum(susceptible_child_bearing[]) / sum(total_child_bearing[]))

births_M <- births_total - births_S

natural_waning[] <- waning * R[i]
dim(natural_waning) <- n_age

vaccine_waning[] <- waning * V[i]
dim(vaccine_waning) <- n_age

loses_maternal <- maternal_waning * M[2] #only wanes in the last stage

#must adjust crude foi for the fact that some people are immune
t_adjusted_foi <- t_crude_foi * sum(total_pop[]) / sum(S[])

infections[] <- t_adjusted_foi * S[i]
dim(infections) <- n_age

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

deaths_M[] <- t_death_rate[i] * M[i]
dim(deaths_M) <- 2

ageing_S[] <- age_rate[i] * S[i]
dim(ageing_S) <- n_age

ageing_R[] <- age_rate[i] * R[i]
dim(ageing_R) <- n_age

ageing_V[] <- age_rate[i] * V[i]
dim(ageing_V) <- n_age

ageing_M[] <- age_rate[i] * M[i]
dim(ageing_M) <- 2

#todo add check against non all or nothing
vaccinations_S[] <- t_vaccination_rate[i] * S[i] * vaccine_efficacy
dim(vaccinations_S) <- n_age

vaccinations_R[] <- t_vaccination_rate[i] * R[i] * vaccine_efficacy
dim(vaccinations_R) <- n_age

vaccinations_M <- t_vaccination_rate[2] * M[2] * vaccine_efficacy

#derivatives
dim(S) <- n_age
initial(S[]) <- S_0[i]
deriv(S[1]) <- births_S + natural_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[2]) <- loses_maternal + ageing_S[i-1] + natural_waning[i] + vaccine_waning[i] - vaccinations_S[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[3]) <- ageing_S[i-1] + ageing_M[2] + natural_waning[i] + vaccine_waning[i] - vaccinations_S[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[4:n_age]) <- ageing_S[i-1] + natural_waning[i] + vaccine_waning[i] - vaccinations_S[i] - ageing_S[i] - infections[i] - deaths_S[i]

dim(R) <- n_age
initial(R[]) <- R_0[i]
deriv(R[1]) <- infections[i] - ageing_R[i] - deaths_R[i] - natural_waning[i]
deriv(R[2:n_age]) <- infections[i] + ageing_R[i-1] - vaccinations_R[i] - ageing_R[i] - deaths_R[i] - natural_waning[i]

dim(V) <- n_age
initial(V[]) <- 0
deriv(V[1]) <- 0
deriv(V[2]) <- vaccinations_S[i] + vaccinations_R[i] + vaccinations_M + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]
deriv(V[3:n_age]) <- vaccinations_S[i] + vaccinations_R[i] + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]

dim(M) <- 2
initial(M[]) <- M_0[i]
deriv(M[1]) <- births_M - ageing_M[1] - deaths_M[1]
deriv(M[2]) <- ageing_M[1] - vaccinations_M - ageing_M[2] - deaths_M[2] - loses_maternal

#output
#tracking doses doesn't make sense in this formulation
