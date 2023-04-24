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

  alpha <- ltm::cronbach.alpha(data)

  rw.data <- select(data,
                    RW.Set.1, RW.Set.2, RW.Set.3, RW.Set.4, RW.Set.5,
                    RW.Atm.1, RW.Atm.2, RW.Atm.3, RW.Atm.4,
                    RW.Iso.1, RW.Iso.2, RW.Iso.3, RW.Iso.4, RW.Iso.5,
                    RW.Sup.1, RW.Sup.2, RW.Sup.3)
  rw.cor.data <- round(cor(rw.data, method = 'pearson'), 2)

  rw.happiness.from.fact <- lm(RW.Set.2 ~ RW.Set.1, data=data)

  model.full <- '
    # "мне нравится быть к/о"
    CO.Sat =~ CO.Sat.1 + CO.Sat.2 + CO.Sat.3
    # "к/о соотв. моим целям"
    CO.N =~ CO.N.1 + CO.N.2
    # "к/о соотв. целям компании (измеряем и фокусируемся)"
    CO.CO =~ CO.CO.1 + CO.CO.2 + CO.CO.3 + CO.CO.4
    # "мы отзывчивы"
    CO.Sen =~ CO.Sen.1 + CO.Sen.2 + CO.Sen.3

    # "я и компания клиентоориентированы"
    CO =~ CO.Sat + CO.N + CO.CO + CO.Sen

    # "коммуникация хороша"
    IMC.Com =~ IMC.Com.1 + IMC.Com.2 + IMC.Com.3
    # "командные встречи хороши" (кроме 1, там скорее негативная шкала)
    IMC.TeamM =~ IMC.TeamM.1 + IMC.TeamM.2 + IMC.TeamM.3 + IMC.TeamM.4 + IMC.TeamM.5 + IMC.TeamM.6 + IMC.TeamM.8 + IMC.TeamM.11 + IMC.TeamM.7 # + IMC.TeamM.9 + IMC.TeamM.10
    # "мессенджеры исп. широко"
    IMC.Msg =~ IMC.Msg.1 + IMC.Msg.2 + IMC.Msg.3 + IMC.Msg.4 + IMC.Msg.5
    # "личные встречи хороши"
    IMC.PersM =~ IMC.PersM.1 + IMC.PersM.2 + IMC.PersM.3 + IMC.PersM.4 + IMC.PersM.5
    # "сетевые связи налажены"
    IMC.Netw =~ IMC.Netw.1 + IMC.Netw.2 + IMC.Netw.3 + IMC.Netw.4 + IMC.Netw.5
    # "внутренняя неформальная коммуникация налажена хорошо"
    IMC.InCom =~ IMC.InCom.1 + IMC.InCom.2 + IMC.InCom.3 + IMC.InCom.4

    # "коммуникация налажена хорошо"
    IMC =~ IMC.Com  + IMC.Msg + IMC.PersM + IMC.Netw + IMC.InCom + IMC.TeamM

    # ""
    IME.Goal =~ IME.Goal.1 + IME.Goal.2 + IME.Goal.3 + IME.Goal.4 + IME.Goal.5 + IME.Goal.6
    # ""
    IME.Act =~ IME.Act.1 + IME.Act.2 + IME.Act.3

    # ""
    IME =~ IME.Goal + IME.Act

    # ""
    IMS.Uti =~ IMS.Uti.1 + IMS.Uti.2 + IMS.Uti.3 + IMS.Uti.4
    # ""
    IMS.Simp =~ IMS.Simp.1 + IMS.Simp.2 + IMS.Simp.3
    # ""
    IMS.Inn =~ IMS.Inn.1 + IMS.Inn.2 + IMS.Inn.3

    # ""
    IMS =~ IMS.Uti + IMS.Simp + IMS.Inn

    # ""
    IMCu.Sat =~ IMCu.Sat.1 + IMCu.Sat.2 + IMCu.Sat.3 + IMCu.Sat.4 + IMCu.Sat.5
    # ""
    IMCu.Cul =~ IMCu.Cul.1 + IMCu.Cul.2 + IMCu.Cul.3 + IMCu.Cul.4 + IMCu.Cul.5 + IMCu.Cul.6 + IMCu.Cul.7 + IMCu.Cul.8 + IMCu.Cul.9 + IMCu.Cul.10 + IMCu.Cul.11
    # ""
    IMCu.Eve =~ IMCu.Eve.1 + IMCu.Eve.2 + IMCu.Eve.3 + IMCu.Eve.4 + IMCu.Eve.5

    # ""
    IMCu =~ IMCu.Sat + IMCu.Eve + IMCu.Cul

    # ""
    IM =~ IMC + IME + IMS #+ IMCu

    # ""
    RW.Set =~ RW.Set.1 + RW.Set.2 + RW.Set.3 + RW.Set.4 + RW.Set.5
    # ""
    RW.Atm =~ RW.Atm.1 + RW.Atm.2 + RW.Atm.3 + RW.Atm.4
    # ""
    RW.Iso =~ RW.Iso.1 + RW.Iso.2 + RW.Iso.3 + RW.Iso.4 + RW.Iso.5 + RW.Iso.6
    # ""
    RW.Sup =~ RW.Sup.1 + RW.Sup.2 + RW.Sup.3

    # ""
    RW =~ RW.Set + RW.Atm + RW.Iso + RW.Sup

    # ""
    #FB =~ FB.1 + FB.3 + FB.4 + FB.5 + FB.6
    # ""
    #SD =~ SD.1 + SD.2 + SD.3 + SD.4
    #


    # ""
    CO ~ IM + RW
    # ""
    IM ~ RW
  '

  fit <- lavaan::sem(model.full, data, estimator = 'ML')
  print('sem')
  lavaan::summary(fit, fit.measures = TRUE, standardized = TRUE)
}

run()
