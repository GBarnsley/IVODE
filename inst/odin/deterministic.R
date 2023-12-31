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

S_births[] <- S[i] * t_birth_rate[i]
dim(S_births) <- n_age

M_births[] <- (R[i] + V[i]) * t_birth_rate[i]
dim(M_births) <- n_age

births_S <- sum(S_births[])

births_M <- sum(M_births[])

natural_waning[] <- waning * R[i]
dim(natural_waning) <- n_age

vaccine_waning[] <- waning * V[i]
dim(vaccine_waning) <- n_age

loses_maternal <- maternal_waning * M[2] #only wanes in the last stage

infections[] <- t_foi[i] * S[i]
dim(infections) <- n_age

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
#vaccine dose calculations (with a numerical limit)
p_vaccinate[1] <-  0
p_vaccinate[2] <- min(t_vaccine_doses[i] / (S[i] + M[i]), 0.9999)
p_vaccinate[3:n_age] <- min(t_vaccine_doses[i] / S[i], 0.9999)
dim(p_vaccinate) <- n_age

#output(temp_p_vaccinate[]) <- p_vaccinate[i]
#dim(temp_p_vaccinate) <- n_age


vaccination_rate[] <- -log(1 - p_vaccinate[i])
dim(vaccination_rate) <- n_age

vaccination_attempts[] <- vaccination_rate[i] * S[i]
dim(vaccination_attempts) <- n_age

vaccinations[] <- vaccination_attempts[i] * vaccine_efficacy
dim(vaccinations) <- n_age

#failed_vaccinations[] <- vaccination_attempts[i] - vaccinations[i]
#dim(failed_vaccinations) <- n_age

maternal_vaccination_attempts <- vaccination_rate[2] * M[2]
maternal_vaccinations <- maternal_vaccination_attempts * vaccine_efficacy
#failed_maternal_vaccinations <- maternal_vaccination_attempts - maternal_vaccinations

#derivatives
dim(S) <- n_age
initial(S[]) <- S_0[i]
deriv(S[1]) <- births_S + natural_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[2]) <- loses_maternal + ageing_S[i-1] + natural_waning[i] + vaccine_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]
deriv(S[3:n_age]) <- ageing_S[i-1] + natural_waning[i] + vaccine_waning[i] - ageing_S[i] - infections[i] - deaths_S[i]

dim(R) <- n_age
initial(R[]) <- R_0[i]
deriv(R[1]) <- infections[i] - ageing_R[i] - deaths_R[i] - natural_waning[i]
deriv(R[2:n_age]) <- infections[i] + ageing_R[i-1] - ageing_R[i] - deaths_R[i] - natural_waning[i]

dim(V) <- n_age
initial(V[]) <- 0
deriv(V[1]) <- 0
deriv(V[2]) <- vaccinations[i] + maternal_vaccinations + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]
deriv(V[3:n_age]) <- vaccinations[i] + ageing_V[i-1] - ageing_V[i] - deaths_V[i] - vaccine_waning[i]

dim(M) <- 2
initial(M[]) <- M_0[i]
deriv(M[1]) <- births_M + vaccine_waning[1] - ageing_M[1] - deaths_M[1]
deriv(M[2]) <- ageing_M[1] + maternal_vaccinations - ageing_M[2] - deaths_M[2] - loses_maternal

#outputs

total_vaccine_doses[1] <- vaccination_attempts[1]
total_vaccine_doses[2] <- vaccination_attempts[2] + maternal_vaccination_attempts
total_vaccine_doses[3:n_age] <- vaccination_attempts[i]
dim(total_vaccine_doses) <- n_age

output(vaccination_doses[]) <- total_vaccine_doses[i]
dim(vaccination_doses) <- n_age
