#install.packages('dplyr')
#install.packages('purrr')
#install.packages('readr')
#install.packages('ltm')
#install.packages('pander')
#install.packages('broom')
#install.packages('lavaan')

library(dplyr)

clean.name <- function(name) {
  indexOfLastDigit <- regexpr('\\d\\.', name)[1]
  res <- substr(name, 0, indexOfLastDigit)
  res <- if (res == '') name else res
  res
}

clean.names <- function(table) {
  old.names <- colnames(table)
  new.names <- unlist(purrr::map(old.names, clean.name))
  data.table::setnames(table, old.names, new.names)
  table
}

without.spare.columns <- function(table) {
  select(table, -FB.2, -X..наблюдения)
}

clean.strings <- function(table) {
  table$RW.Set.1 <- unlist(purrr::map(table$RW.Set.1, function (x) {
    days <- substr(x, 1, 1);
    if (days == 'Н') {
      days <- '0'
    }
    unlist(readr::parse_integer(days));
  }));
  na.as.zero <- function (x) {
    if (is.na(x)) 0 else x
  };
  table$FB.2 <- unlist(purrr::map(table$FB.2, na.as.zero));
  table$FB.3 <- unlist(purrr::map(table$FB.3, na.as.zero));
  table$FB.4 <- unlist(purrr::map(table$FB.4, na.as.zero));
  table$FB.5 <- unlist(purrr::map(table$FB.5, na.as.zero));
  table$FB.6 <- unlist(purrr::map(table$FB.6, na.as.zero));
  table$FB.1 <- unlist(purrr::map(table$FB.1, function (x) {
    if (x == 'Да') 2 else 1;
  }));

  table$SD.1 <- unlist(purrr::map(table$SD.1, function (x) {
    if (x == 'Мужской') 1 else 2;
  }));

  table$SD.2 <- unlist(purrr::map(table$SD.2, function (x) {
    if (x == '18-25 лет') 1
    else if (x == '26-35 лет') 2
    else if (x == '36-45 лет') 3
    else if (x == '46-55 лет') 4
    else 5;
  }));

  table$SD.3 <- unlist(purrr::map(table$SD.3, function (x) {
    if (x == 'Неоконченное высшее') 1
    else if (x == 'Высшее или степень магистра') 2
    else if (x == 'Ученая степень') 3
    else 4;
  }));

  table$SD.4 <- unlist(purrr::map(table$SD.4, function (x) {
    if (x == 'Исследования') 1
    else if (x == 'Владелец продукта / менеджер') 2
    else if (x == 'Анализ данных (в т.ч. использование AI)') 3
    else if (x == 'Разработка (в т.ч. тестирование и сопровождение)') 4
    else if (x == 'Ученая степень') 5
    else 0;
  }));

  table
}

run <- function() {
  src <- read.csv('src.csv', sep = ';')
  src <- clean.names(src);
  src <- clean.strings(src)
  data <- without.spare.columns(src)
  write.table(data, 'processed.csv', sep = ';')
  print('Data:')
  print(data)
  alpha <- ltm::cronbach.alpha(data)

  rw.data <- select(data,
                    RW.Set.1, RW.Set.2, RW.Set.3, RW.Set.4, RW.Set.5,
                    RW.Atm.1, RW.Atm.2, RW.Atm.3, RW.Atm.4,
                    RW.Iso.1, RW.Iso.2, RW.Iso.3, RW.Iso.4, RW.Iso.5,
                    RW.Sup.1, RW.Sup.2, RW.Sup.3)
  rw.cor.data <- round(cor(rw.data, method = 'pearson'), 2)
  print('Pearson correlation:')
  print(rw.cor.data)

  rw.happiness.from.fact <- lm(RW.Set.2 ~ RW.Set.1, data=data)
  print('Linear regression RW happiness from RW fact:')
  print(rw.happiness.from.fact)

  model.full <- '
    CO =~ CO.Sat.1 + CO.Sat.2 + CO.Sat.3 + CO.N.1 + CO.N.2 + CO.CO.1 + CO.CO.2 + CO.CO.3 + CO.CO.4 + CO.Sen.1 + CO.Sen.2 + CO.Sen.3
    IMC =~ IMC.Com.1 + IMC.Com.2 + IMC.Com.3 + IMC.TeamM.1 + IMC.TeamM.2 + IMC.TeamM.3 + IMC.TeamM.4 + IMC.TeamM.5 + IMC.TeamM.6 + IMC.TeamM.7 + IMC.TeamM.8 + IMC.TeamM.9 + IMC.TeamM.10 + IMC.TeamM.11 + IMC.Msg.1 + IMC.Msg.2 + IMC.Msg.3 + IMC.Msg.4 + IMC.Msg.5 + IMC.PersM.1 + IMC.PersM.2 + IMC.PersM.3 + IMC.PersM.4 + IMC.PersM.5 + IMC.Netw.1 + IMC.Netw.2 + IMC.Netw.3 + IMC.Netw.4 + IMC.Netw.5 + IMC.InCom.1 + IMC.InCom.2 + IMC.InCom.3 + IMC.InCom.4
    RW =~ RW.Set.1 + RW.Set.2 + RW.Set.3 + RW.Set.4 + RW.Set.5 + RW.Atm.1 + RW.Atm.2 + RW.Atm.3 + RW.Atm.4 + RW.Iso.1 + RW.Iso.2 + RW.Iso.3 + RW.Iso.4 + RW.Iso.5 + RW.Sup.1 + RW.Sup.2 + RW.Sup.3

    CO ~ IMC + RW
    IMC ~ RW
  '
  model.lite <- '
    CO =~ CO.Sat.1 + CO.Sat.2 + CO.Sat.3
    IMC =~ IMC.Com.1 + IMC.Com.2 + IMC.Com.3
    RW =~ RW.Set.1 + RW.Set.2 + RW.Set.3 + RW.Set.4 + RW.Set.5

    CO ~ IMC + RW
    IMC ~ RW
  '
  fit <- lavaan::sem(model.full, data)
  print('sem')
  lavaan::summary(fit, standardized = TRUE)
}

run()
