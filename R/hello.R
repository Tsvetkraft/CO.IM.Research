#install.packages('dplyr')
#install.packages('purrr')
#install.packages('readr')
#install.packages('ltm')
#install.packages('pander')
#install.packages('broom')
#install.packages('lavaan')
#install.packages('checkmate')

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
                    RW.Set.1,
                    RW.Iso.1, RW.Iso.2, RW.Iso.3, RW.Iso.4, RW.Iso.5)
  rw.cor.data <- round(cor(rw.data, method = 'pearson'), 2)
  #print(rw.cor.data)

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
    # "командные встречи хороши"
    IMC.TeamM =~ IMC.TeamM.2 + IMC.TeamM.3 + IMC.TeamM.4 + IMC.TeamM.5 + IMC.TeamM.6 + IMC.TeamM.8 + IMC.TeamM.10 + IMC.TeamM.7 # IMC.TeamM.1 + IMC.TeamM.9 + IMC.TeamM.11
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

    # "обучение помогает работе"
    IME.Goal =~ IME.Goal.1 + IME.Goal.2 + IME.Goal.3 + IME.Goal.4 + IME.Goal.5 + IME.Goal.6
    # "обучение актуализируется со временем"
    IME.Act =~ IME.Act.1 + IME.Act.2 + IME.Act.3

    # "обучение налажено хорошо"
    IME =~ IME.Goal + IME.Act

    # "цифровые сервисы полезны"
    IMS.Uti =~ IMS.Uti.1 + IMS.Uti.2 + IMS.Uti.3 + IMS.Uti.4
    # "цифровые сервисы просты и удобны"
    IMS.Simp =~ IMS.Simp.1 + IMS.Simp.2 + IMS.Simp.3
    # "я реально инноватор"
    IMS.Inn =~ IMS.Inn.1 + IMS.Inn.2 + IMS.Inn.3

    # "цифровые сервисы хороши"
    IMS =~ IMS.Uti + IMS.Simp + IMS.Inn

    # "культура направлена на удовлетворение сотрудника"
    IMCu.Sat =~ IMCu.Sat.1 + IMCu.Sat.2 + IMCu.Sat.3 + IMCu.Sat.4 + IMCu.Sat.5

    # "мы сплочены и семейны"
    IMCu.Cul.AllTogether =~ IMCu.Cul.1 + IMCu.Cul.2 + IMCu.Cul.3 + IMCu.Cul.4 + IMCu.Cul.5
    # "мы иерархичны и конкуретны"
    #IMCu.Cul.HierarchyCompetition =~ IMCu.Cul.6 + IMCu.Cul.7 + IMCu.Cul.8 + IMCu.Cul.9 + IMCu.Cul.10 + IMCu.Cul.11
    # "горизонтальная культура"
    IMCu.Cul =~ IMCu.Cul.AllTogether# + IMCu.Cul.HierarchyCompetition

    # "корпмероприятия хороши"
    IMCu.Eve =~ IMCu.Eve.1 + IMCu.Eve.2 + IMCu.Eve.3 + IMCu.Eve.4 + IMCu.Eve.5

    # "культура хороша"
    IMCu =~ IMCu.Sat + IMCu.Eve + IMCu.Cul

    # "внутренний маркетинг хорош"
    IM =~ IMC + IME + IMS + IMCu

    # "работаю 0 - 5 дней удаленно"
    RW.Set.Qty =~ RW.Set.1
    # "удаленка нравится и эффективна"
    RW.Set =~ RW.Set.2 + RW.Set.3 + RW.Set.4 + RW.Set.5
    # "удаленка неудобна и неэффективна" (!)
    #RW.Atm.Bad =~ RW.Atm.1 + RW.Atm.2 + RW.Atm.3 + RW.Atm.4
    # "удаленка хорошая и инструменты интересные"
    RW.Atm.Good =~ RW.Atm.3 + RW.Atm.4
    # "я чувствую себя изолированным и потерянным на удаленке"
    RW.Iso =~ RW.Iso.1 + RW.Iso.2 + RW.Iso.3 + RW.Iso.4 + RW.Iso.5 + RW.Iso.6
    # "компания помогает мне обустроить удаленку"
    RW.Sup =~ RW.Sup.1 + RW.Sup.2 + RW.Sup.3

    # "удаленка эффективная?" TODO
    RW =~ RW.Set.Qty + RW.Set + RW.Atm.Good + RW.Sup # + RW.Atm.Bad

    # "работаю с ОС"
    FB.Has =~ FB.1
    # "ОС мотивирует"
    FB.Motivates =~ FB.4 + FB.5 + FB.6
    # "ОС полезная и влияет на КО"
    FB =~ FB.Has + FB.Motivates

    #SD =~ SD.1 + SD.2 + SD.3 + SD.4
    #


    # ""
    CO ~ IM
    CO ~ RW.Iso
    CO ~ FB.Motivates
    # CO ~ IM + RW
    # ""
    IM ~ RW
    IM ~ RW.Iso
  '

  #fit <- lavaan::sem(model.full, data, estimator = 'ML')
  #print('standardizedSolution')
  #lavaan::standardizedSolution(fit)
  fit <- lavaan::cfa(model.full, data, estimator = 'DWLS')
  print('summary')
  lavaan::summary(fit, fit.measures = TRUE, standardized = TRUE)

  semPlot::semPaths(fit, what = 'est', layout = 'tree2', intercepts = TRUE, residuals = TRUE, thresholds = TRUE)
}

run()

