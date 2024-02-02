#inputs
n_age <- user()
n_maternal <- user()

child_bearing[] <- user()
dim(child_bearing) <- n_age

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

adjust_for_crude_foi <- user()

t_crude_foi <- interpolate(tt_crude_foi, crude_foi, "constant")

M_0[] <- user()
dim(M_0) <- n_maternal

S_0[] <- user()
dim(S_0) <- n_age

R_0[] <- user()
dim(R_0) <- n_age

V_0[] <- user()
dim(V_0) <- n_age

VD_0[] <- user()
dim(VD_0) <- n_age


#transitions
total_pop[1:n_maternal] <- R[i] + S[i] + V[i] + M[i] + VD[i]
total_pop[(n_maternal + 1):n_age] <- R[i] + S[i] + V[i] + VD[i]
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

#adjust death rate so that it matches crude rate
weighted_totals[] <- prop_death[i] * total_pop[i]
dim(weighted_totals) <- n_age

t_death_rate[] <- min(t_crude_death_rate * prop_death[i] * sum(total_pop[]) / sum(weighted_totals[]), 1)
dim(t_death_rate) <- n_age

births_deaths_S[1] <- births_S - t_death_rate[i] * S[i]
births_deaths_S[2:n_age] <- -t_death_rate[i] * S[i]
dim(births_deaths_S) <- n_age

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

#must adjust crude foi for the fact that some people are immune

#this can't go over 1 else the mathematics breaks, also crude_foi can't be greater than S/N at any point else this limit is met.
t_adjusted_foi <- if (adjust_for_crude_foi) min(t_crude_foi * sum(total_pop[]) / sum(susceptible_pop[]), 1) else t_crude_foi

infections_S[] <- t_adjusted_foi * S[i]
dim(infections_S) <- n_age

infections_VD[] <- t_adjusted_foi * VD[i]
dim(infections_VD) <- n_age

gains_immunity[] <- infections_S[i] + infections_VD[i]
dim(gains_immunity) <- n_age

#ageing and vaccinations happen externally

#derivatives
dim(S) <- n_age
initial(S[]) <- S_0[i]
deriv(S[]) <- births_deaths_S[i] + loses_immunity[i] - infections_S[i]

dim(R) <- n_age
initial(R[]) <- R_0[i]
deriv(R[]) <- births_deaths_R[i] + gains_immunity[i] - waning_R[i]

dim(V) <- n_age
initial(V[]) <- V_0[i]
deriv(V[]) <- births_deaths_V[i] - waning_V[i]

dim(VD) <- n_age
initial(VD[]) <- VD_0[i]
deriv(VD[]) <- births_deaths_VD[i] - waning_VD[i] - infections_VD[i]

dim(M) <- n_maternal
initial(M[]) <- M_0[i]
deriv(M[]) <- births_deaths_M[i]
