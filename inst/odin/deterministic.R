#inputs
n_age <- user()

age_rate[] <- user()
dim(age_rate) <- n_age

tt_death_rate[] <- user()
dim(tt_death_rate) <- user()

death_rate[,] <- user()
dim(death_rate) <- c(length(tt_death_rate), n_age)

t_death_rate[] <- interpolate(tt_death_rate, death_rate, "constant")
dim(t_death_rate) <- n_age

tt_birth_rate[] <- user()
dim(tt_birth_rate) <- user()

birth_rate[,] <- user()
dim(birth_rate) <- c(length(tt_birth_rate), n_age)

t_birth_rate[] <- interpolate(tt_birth_rate, birth_rate, "constant")
dim(t_birth_rate) <- n_age

maternal_waning <- user()

waning <- user()

tt_foi[] <- user()
dim(tt_foi) <- user()

foi[,] <- user()
dim(foi) <- c(length(tt_foi), n_age)

t_foi[] <- interpolate(tt_foi, foi, "constant")
dim(t_foi) <- n_age

vaccine_efficacy <- user()

vaccine_efficacy_disease <- user()

vaccine_efficacy_disease_adjusted <- (vaccine_efficacy_disease - vaccine_efficacy) / (1 - vaccine_efficacy)

tt_vaccine_doses[] <- user()
dim(tt_vaccine_doses) <- user()

vaccine_doses[,] <- user()
dim(vaccine_doses) <- c(length(tt_vaccine_doses), n_age)

t_vaccine_doses[] <- interpolate(tt_vaccine_doses, vaccine_doses, "constant")
dim(t_vaccine_doses) <- n_age

M_0[] <- user()
dim(M_0) <- 2

S_0[] <- user()
dim(S_0) <- n_age

R_0[] <- user()
dim(R_0) <- n_age

#tranistions

S_births[] <- (S[i] + VD[i]) * t_birth_rate[i]
dim(S_births) <- n_age

M_births[] <- (R[i] + V[i]) * t_birth_rate[i]
dim(M_births) <- n_age

births_S <- sum(S_births[])

births_M <- sum(M_births[])

natural_waning[] <- waning * R[i]
dim(natural_waning) <- n_age

vaccine_waning[] <- waning * V[i]
dim(vaccine_waning) <- n_age

vaccine_partial_waning[] <- waning * VD[i]
dim(vaccine_partial_waning) <- n_age

loses_maternal <- maternal_waning * M[2] #only wanes in the last stage

infections[] <- t_foi[i] * S[i]
dim(infections) <- n_age

infections_VD[] <- t_foi[i] * VD[i]
dim(infections_VD) <- n_age

deaths_S[] <- t_death_rate[i] * S[i]
dim(deaths_S) <- n_age

deaths_R[] <- t_death_rate[i] * R[i]
dim(deaths_R) <- n_age

deaths_V[] <- t_death_rate[i] * V[i]
dim(deaths_V) <- n_age

deaths_VD[] <- t_death_rate[i] * VD[i]
dim(deaths_VD) <- n_age

deaths_M[] <- t_death_rate[i] * M[i]
dim(deaths_M) <- 2

ageing_S[] <- age_rate[i] * S[i]
dim(ageing_S) <- n_age

ageing_R[] <- age_rate[i] * R[i]
dim(ageing_R) <- n_age

ageing_V[] <- age_rate[i] * V[i]
dim(ageing_V) <- n_age

ageing_VD[] <- age_rate[i] * VD[i]
dim(ageing_VD) <- n_age

ageing_M[] <- age_rate[i] * M[i]
dim(ageing_M) <- 2

#todo add check against non all or nothing
#vaccine dose calculations (with a numerical limit)
p_vaccinate[1] <-  0
p_vaccinate[2] <- min(t_vaccine_doses[i] / (S[i] + R[i] + M[i]), 0.9999)
p_vaccinate[3:n_age] <- min(t_vaccine_doses[i] / (S[i] + R[i]), 0.9999)
dim(p_vaccinate) <- n_age

#output(temp_p_vaccinate[]) <- p_vaccinate[i]
#dim(temp_p_vaccinate) <- n_age


vaccination_rate[] <- -log(1 - p_vaccinate[i])
dim(vaccination_rate) <- n_age

vaccination_attempts_S[] <- vaccination_rate[i] * S[i]
dim(vaccination_attempts_S) <- n_age

vaccinations_S[] <- vaccination_attempts_S[i] * vaccine_efficacy
dim(vaccinations_S) <- n_age

vaccinations_partial_S[] <- (vaccination_attempts_S[i] - vaccinations_S[i]) * vaccine_efficacy_disease_adjusted
dim(vaccinations_partial_S) <- n_age

vaccination_attempts_R[] <- vaccination_rate[i] * R[i]
dim(vaccination_attempts_R) <- n_age

vaccinations_R[] <- vaccination_attempts_R[i] * vaccine_efficacy
dim(vaccinations_R) <- n_age

vaccination_attempts_M <- vaccination_rate[2] * M[2]
vaccinations_M <- vaccination_attempts_M * vaccine_efficacy

vaccinations_partial_M <- (vaccination_attempts_M - vaccinations_M) * vaccine_efficacy_disease_adjusted

#derivatives
dim(S) <- n_age
initial(S[]) <- S_0[i]
deriv(S[1]) <- births_S + natural_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[2]) <- loses_maternal + ageing_S[i-1] + natural_waning[i] + vaccine_waning[i] + vaccine_partial_waning[i] - vaccinations_S[i] - vaccinations_partial_S[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[3]) <- ageing_S[i-1] + ageing_M[2] + natural_waning[i] + vaccine_waning[i] + vaccine_partial_waning[i] - vaccinations_S[i] - vaccinations_partial_S[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[4:n_age]) <- ageing_S[i-1] + natural_waning[i] + vaccine_waning[i] + vaccine_partial_waning[i] - vaccinations_S[i] - vaccinations_partial_S[i] - ageing_S[i] - infections[i] - deaths_S[i]

dim(R) <- n_age
initial(R[]) <- R_0[i]
deriv(R[1]) <- infections[i] + infections_VD[i] - ageing_R[i] - deaths_R[i] - natural_waning[i]
deriv(R[2:n_age]) <- infections[i] + infections_VD[i] + ageing_R[i-1] - vaccinations_R[i] - ageing_R[i] - deaths_R[i] - natural_waning[i]

dim(V) <- n_age
initial(V[]) <- 0
deriv(V[1]) <- 0
deriv(V[2]) <- vaccinations_S[i] + vaccinations_R[i] + vaccinations_M + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]
deriv(V[3:n_age]) <- vaccinations_S[i] + vaccinations_R[i] + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]

dim(VD) <- n_age
initial(VD[]) <- 0
deriv(VD[1]) <- 0
deriv(VD[2]) <- vaccinations_partial_S[i] + vaccinations_partial_M + ageing_VD[i-1] - infections_VD[i] - ageing_VD[i] - deaths_VD[i] - vaccine_partial_waning[i]
deriv(VD[3:n_age]) <- vaccinations_partial_S[i] + ageing_VD[i-1] - infections_VD[i] - ageing_VD[i] - deaths_VD[i] - vaccine_partial_waning[i]

dim(M) <- 2
initial(M[]) <- M_0[i]
deriv(M[1]) <- births_M - ageing_M[1] - deaths_M[1]
deriv(M[2]) <- ageing_M[1] - vaccinations_M - vaccinations_partial_M - ageing_M[2] - deaths_M[2] - loses_maternal

#outputs

total_vaccine_doses[1] <- vaccination_attempts_S[1] + vaccination_attempts_R[1]
total_vaccine_doses[2] <- vaccination_attempts_S[2] + vaccination_attempts_R[2] + vaccination_attempts_M
total_vaccine_doses[3:n_age] <- vaccination_attempts_S[i] + vaccination_attempts_R[i]
dim(total_vaccine_doses) <- n_age

output(vaccination_doses[]) <- total_vaccine_doses[i]
dim(vaccination_doses) <- n_age