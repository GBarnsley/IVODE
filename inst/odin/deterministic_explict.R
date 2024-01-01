#inputs
n_age <- user()
n_vacc <- 3 #1 unvacc, 2 failed, 3 vacc

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

M_0[,] <- user()
dim(M_0) <- c(2, n_vacc)

S_0[,] <- user()
dim(S_0) <- c(n_age, n_vacc)

R_0[,] <- user()
dim(R_0) <- c(n_age, n_vacc)

#tranistions

S_births[,] <- S[i, j] * t_birth_rate[i]
dim(S_births) <- c(n_age, 2)

M_births[] <- (R[i, 3]) * t_birth_rate[i]
dim(M_births) <- n_age

births_S <- sum(S_births[,])

births_M <- sum(M_births[])

natural_waning[,] <- waning * R[i, j]
dim(natural_waning) <- c(n_age, n_vacc)

vaccine_waning_S[] <- waning * S[i, 3]
dim(vaccine_waning_S) <- n_age

vaccine_waning_R[] <- waning * R[i, 3]
dim(vaccine_waning_R) <- n_age

loses_maternal[] <- maternal_waning * M[2, i] #only wanes in the last stage
dim(loses_maternal) <- n_vacc

infections[,] <- t_foi[i] * S[i, j]
dim(infections) <- c(n_age, 2)

deaths_S[,] <- t_death_rate[i] * S[i, j]
dim(deaths_S) <- c(n_age, n_vacc)

deaths_R[,] <- t_death_rate[i] * R[i, j]
dim(deaths_R) <- c(n_age, n_vacc)

deaths_M[,] <- t_death_rate[i] * M[i, j]
dim(deaths_M) <- c(n_age, 2)


ageing_S[,1] <- age_rate[i] * (S[i, 1] + S[i, 2])
ageing_S[,2] <- 0
ageing_S[,3] <- age_rate[i] * S[i, 3]
dim(ageing_S) <- c(n_age, n_vacc)

ageing_R[,1] <- age_rate[i] * (R[i, 1] + R[i, 2])
ageing_R[,2] <- 0
ageing_R[,3] <- age_rate[i] * R[i, 3]
dim(ageing_R) <- c(n_age, n_vacc)

ageing_M[,1] <- age_rate[i] * (M[i, 1] + M[i, 2])
ageing_M[,2] <- 0
ageing_M[,3] <- age_rate[i] * M[i, 3]
dim(ageing_M) <- c(n_age, 2)

ageing[1, 1, 1] <- births_S
ageing[2, 1, 1] <- 0
ageing[1:2, 1, 2:n_vacc] <- 0
ageing[1, 2, 1] <- 
ageing[2, 2, 1] <- 
ageing[1:2, 2, 2:n_vacc] <- 


ageing[1:2, 2:n_age, 1:n_vacc] <- (age_rate[j-1] * C[i, j-1, k]) - (age_rate[j] * C[i, j, k])


ageing[1:2, 2:n_age, 1:n_vacc] <- (age_rate[j-1] * C[i, j-1, k]) - (age_rate[j] * C[i, j, k])
dim(ageing) <- c(2, n_age, n_vacc)


#todo add check against non all or nothing
#vaccine dose calculations (with a numerical limit)
p_vaccinate[1] <-  0
p_vaccinate[2] <- min(t_vaccine_doses[i] / (S[i, 1] + R[i, 1] + M[i, 1]), 0.9999)
p_vaccinate[3:n_age] <- min(t_vaccine_doses[i] / (S[i, 1] + R[i, 1]), 0.9999)
dim(p_vaccinate) <- n_age

vaccination_rate[] <- -log(1 - p_vaccinate[i])
dim(vaccination_rate) <- n_age

vaccination_attempts_S[] <- vaccination_rate[i] * S[i, 1]
dim(vaccination_attempts_S) <- n_age

vaccinations_S[] <- vaccination_attempts_S[i] * vaccine_efficacy
dim(vaccinations_S) <- n_age

vaccination_attempts_R[] <- vaccination_rate[i] * R[i, 1]
dim(vaccination_attempts_R) <- n_age

vaccinations_R[] <- vaccination_attempts_R[i] * vaccine_efficacy
dim(vaccinations_R) <- n_age

vaccination_attempts_M <- vaccination_rate[2] * M[2, 1]
vaccinations_M <- vaccination_attempts_M * vaccine_efficacy

#derivatives
dim(C) <- c(2, n_age, n_vacc)
initial(C[1,,]) <- S_0[i, j]
initial(C[2,,]) <- R_0[i, j]


dim(S) <- c(n_age, n_vacc)
initial(S[,]) <- S_0[i, j]
deriv(S[1, 1]) <- births_S + natural_waning[i, j] - ageing_S[i, j] - infections[i, j] - deaths_S[i, j]
deriv(S[2, 1]) <- ageing_S[i - 1, j] + natural_waning[i, j] - ageing_S[i, j] - infections[i, j] - deaths_S[i, j]
deriv(S[3:n_age, 1]) <- ageing_S[i - 1, j] + natural_waning[i, j] - ageing_S[i, j] - infections[i, j] - deaths_S[i, j]
deriv(S[1, 2]) <- natural_waning[i, j] - ageing_S[i, j] - infections[i, j] - deaths_S[i, j]
deriv(S[2, 2]) <- ageing_S[i - 1, j] + natural_waning[i, j] - ageing_S[i, j] - infections[i, j] - deaths_S[i, j]
deriv(S[3:n_age, 2]) <- ageing_S[i - 1, j] + natural_waning[i, j] - ageing_S[i, j] - infections[i, j] - deaths_S[i, j]
deriv(S[1, 3]) <- natural_waning[i, j] - ageing_S[i, j] - deaths_S[i, j]
deriv(S[2, 3]) <- ageing_S[i - 1, j] + natural_waning[i, j] - ageing_S[i, j] - deaths_S[i, j]
deriv(S[3:n_age, 3]) <- ageing_S[i - 1, j] + natural_waning[i, j] - ageing_S[i, j] - deaths_S[i, j]

dim(R) <- c(n_age, n_vacc)
initial(R[,]) <- R_0[i, j]
deriv(R[1, 1]) <- infections[i, j] - ageing_R[i, j] - natural_waning[i, j] - deaths_R[i, j]
deriv(R[2:n_age, 1]) <- ageing_R[i - 1, j] + infections[i, j] - ageing_R[i, j] - natural_waning[i, j] - deaths_R[i, j]
deriv(R[1, 2]) <- infections[i, j] - ageing_R[i, j] - natural_waning[i, j] - deaths_R[i, j]
deriv(R[2:n_age, 2]) <- ageing_R[i - 1, j] + infections[i, j] - ageing_R[i, j] - natural_waning[i, j] - deaths_R[i, j]
deriv(R[1, 3]) <- - ageing_R[i, j] - natural_waning[i, j] - deaths_R[i, j]
deriv(R[2:n_age, 3]) <- ageing_R[i - 1, j] - ageing_R[i, j] - natural_waning[i, j] - deaths_R[i, j]

dim(M) <- c(2, n_vacc)
initial(M[,]) <- M_0[i, j]
deriv(M[1, 1]) <- births_M - ageing_M[i, j] - deaths_M[i, j]
deriv(M[2, 1]) <- ageing_M[i - 1, j] - loses_maternal[j] - ageing_M[i, j] - deaths_M[i, j]
deriv(M[1, 2]) <- 0
deriv(M[2, 2]) <- ageing_M[i - 1, j] - loses_maternal[j] - ageing_M[i, j] - deaths_M[i, j]
deriv(M[1, 3]) <- 0
deriv(M[2, 3]) <- ageing_M[i - 1, j] - loses_maternal[j] - ageing_M[i, j] - deaths_M[i, j]


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

#outputs

total_vaccine_doses[1] <- vaccination_attempts_S[1] + vaccination_attempts_R[1]
total_vaccine_doses[2] <- vaccination_attempts_S[2] + vaccination_attempts_R[2] + vaccination_attempts_M
total_vaccine_doses[3:n_age] <- vaccination_attempts_S[i] + vaccination_attempts_R[i]
dim(total_vaccine_doses) <- n_age

output(vaccination_doses[]) <- total_vaccine_doses[i]
dim(vaccination_doses) <- n_age
