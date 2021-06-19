# Backtesting Module

# Initial settings including loading data from DB and SSL from local/cloud

#' @export
backtest_portfolio =
  function(test_title="Portfolio Return", ssl_list, top_n, pred_col, SN_ratio, seed_money, upper_bound, lower_bound, start_date = '2017-01-01', end_date = '9999-12-31') {

    if(length(top_n) != length(ssl_list)) {
      stop("top_n length must be equal to ssl_list length")
    }
    if(length(pred_col) != length(ssl_list)) {
      stop("pred_col length must be equal to ssl_list length")
    }
    if(length(SN_ratio) != length(ssl_list)) {
      stop("SN_ratio length must be equal to ssl_list length. If you don't wanted to use this argument, use 1 instead")
    }
    if(length(seed_money) != length(ssl_list)) {
      stop("seed_money length must be equal to ssl_list length. If you don't wanted to use this argument, use NA instead")
    }
    if(length(upper_bound) != length(ssl_list)) {
      stop("upper_bound length must be equal to ssl_list length")
    }
    if(length(lower_bound) != length(ssl_list)) {
      stop("lower_bound length must be equal to ssl_list length")
    }

    upper_bound[is.na(upper_bound)] = 999999
    lower_bound[is.na(lower_bound)] = -999999

    if(sum(upper_bound <= 0) > 0) {
      stop("Upper selling bound must be greater than Zero")
    }
    if(sum(lower_bound >= 0) > 0) {
      stop("Lower selling lower bound must be less than Zero")
    }


    tic()

    rets_save <- data.frame()

    d_kospi_kosdaq_cum <-
      d_kospi_kosdaq %>%
      filter(date >= start_date) %>%
      filter(date <= end_date) %>%
      mutate(kospi_cumret = cumprod(kospi+1)-1, kosdaq_cumret = cumprod(kosdaq+1)-1)

    for (l in 1:length(ssl_list)) {
      ssl_list[[l]] <- ssl_list[[l]] %>% mutate(date = ymd(date)) %>% filter(date >= start_date) %>% filter(date <= end_date)
    }

    for (l in 1:length(ssl_list)) {
      ssl <-
        ssl_list[[l]] %>%
        group_by(date) %>%
        arrange(desc(get(pred_col[l])), .by_group = TRUE) %>%
        mutate(stock_cd = str_pad(stock_cd, 6,side = c('left'), pad = '0')) %>%
        ungroup()

      rets_cum <- data.frame()
      monthly_win_vec <- c()
      risk_ratio_vec <- c()
      daily_rets_df <- c()

      for(i in c(1 : ((ssl$date %>% unique %>% length)))) {

        # Sector 중립
        SN_limit = floor(top_n[l]*SN_ratio[l])

        sector_temp <-
          ssl %>%
          select(date, stock_cd, pred_col[l]) %>%
          filter(date == unique(ssl$date)[i]) %>%
          left_join(., sector_info %>% select(stock_cd, sector), by="stock_cd") %>%
          group_by(sector) %>%
          arrange(desc(get(pred_col[l]))) %>%
          dplyr::slice(1:SN_limit)

        ssl_sn <-
          ssl %>%
          filter(stock_cd %in% sector_temp$stock_cd) %>%
          group_by(date) %>%
          arrange(desc(get(pred_col[l])), .by_group = TRUE) %>%
          ungroup()

        rets_temp <-
          d_stock_price %>%
          filter(date >= unique(ssl$date)[i]) %>%
          filter(date <= end_date) %>%
          filter(date <= ifelse(i == length(unique(ssl$date)),
                                d_stock_price %>%
                                  select(date) %>%
                                  unique() %>%
                                  filter(substr(date, 1, 7) == max(substr(date, 1, 7))) %>%
                                  mutate(id=row_number()) %>%
                                  filter(id <= 3) %$%
                                  max(date),
                                unique(ssl$date)[i + 1])) %>%
          filter(stock_cd %in% ssl_sn$stock_cd[ssl_sn$date==(unique(ssl_sn$date)[i])][1:top_n[l]]) %>%
          spread(key = "stock_cd",
                 value = "price")

        names(rets_temp)[-1] <- paste0("stock", c(1 : top_n[l]))

        rets_temp <-
          left_join(data.frame(date = d_stock_price %>%
                                 filter(date >= ymd(unique(ssl$date)[i])) %>%
                                 filter(date <= end_date) %>%
                                 filter(date <= ifelse(i == length(unique(ssl$date)),
                                                       d_stock_price %>%
                                                         select(date) %>%
                                                         unique() %>%
                                                         filter(substr(date, 1, 7) == max(substr(date, 1, 7))) %>%
                                                         mutate(id=row_number()) %>%
                                                         filter(id <= 3) %$%
                                                         max(date),
                                                       unique(ssl$date)[i + 1])) %$%
                                 date %>% unique), rets_temp, by = 'date')

        # 시드머니 시뮬레이션
        if (is.na(seed_money[l]) == FALSE) {
          each_stock_cap = seed_money[l] / top_n[l]
          price_temp = rets_temp[nrow(rets_temp),] %>% select(-date)
          each_stock_cnt = floor(each_stock_cap / price_temp)
          price_sum_temp = price_temp * each_stock_cnt
          surplus = seed_money[l] - sum(price_sum_temp) %>% as.numeric()

          each_stock_cnt

          k=1
          while (surplus > min(price_temp)) {

            if (k > top_n[l]) {
              k = 1
            } else {
              k = k
            }
            if(surplus >= price_temp[k]) {
              each_stock_cnt[k] = each_stock_cnt[k] + 1
              surplus = surplus - price_temp[k]
              k = k + 1
            } else {
              k = k + 1
            }
          }
        }

        # 수익률 계산
        rets_base <- rets_temp
        for (s in 1:top_n[l]) {
          rets_base <- cbind(rets_base, rets_temp %$% get(paste0('stock', s))[1])
          colnames(rets_base)[ncol(rets_base)] <- paste0("base", s)
          rets_base %<>% mutate(return_temp = ( (get(paste0('stock',s)) * 0.9972) -get(paste0('base',s)) ) / get(paste0('base',s)))
          colnames(rets_base)[ncol(rets_base)] <- paste0("return", s)
        }

        active_rets_temp <- rets_base %>% select(date, contains('return'))

        # ==== Active Trading ====
        active_rets = active_rets_temp %>% select(-date)
        idx <- ifelse(apply(t(apply(active_rets, 1, function(x){x > upper_bound[l] | x < lower_bound[l]})), 2, which.max) == 1 | apply(t(apply(active_rets, 1, function(x){x > upper_bound[l] | x < lower_bound[l]})), 2, which.max) == nrow(active_rets_temp), -1, apply(t(apply(active_rets, 1, function(x){x > upper_bound[l] | x < lower_bound[l]})), 2, which.max)) + 1
        for (i in 1:ncol(active_rets)) {
          if (idx[i] != 0) {
            active_rets[idx[i]:nrow(active_rets),i] <- NA
          }
        }
        active_rets <- na.locf(active_rets)
        active_rets
        # ==== Active Trading ====

        rets_cum_temp = cbind(date=active_rets_temp[,"date"], active_rets)

        portfolio.returns <- c()

        if (is.na(seed_money[l]) == TRUE) {
          for(i in c(1 : nrow(rets_cum_temp))){
            k = rets_cum_temp[i,-1] %>% as.matrix %>% as.numeric
            k_ret = mean(k)
            portfolio.returns <- c(portfolio.returns, k_ret) ; rm(k, k_ret)
          }
        } else if (is.na(seed_money[l]) == FALSE) {
          for(i in c(1 : nrow(rets_cum_temp))){
            k = cbind(rets_cum_temp[i,-1], returncash = 0) %>% as.matrix %>% as.numeric
            k_ret = weighted.mean(k, cbind(each_stock_cnt*price_temp, surplus=surplus))
            portfolio.returns <- c(portfolio.returns, k_ret) ; rm(k, k_ret)
          }
        }

        # Daily Return
        daily_rets <-
          rets_temp %>%
          select(-date) %>%
          xts(order.by = rets_temp$date) %>%
          Return.calculate() %>%
          na.omit() %>%
          as.data.frame() %>%
          rowMeans()

        daily_rets_df <- c(daily_rets_df, daily_rets)

        # 월별 승률
        monthly_win_yn <-
          d_kospi_kosdaq_cum %>%
          filter(date %in% rets_cum_temp$date) %>%
          select(date, kospi) %>%
          dplyr::slice(-1) %>%
          mutate(kospi_monthly_ret = cumprod(1+kospi)-1) %$%
          kospi_monthly_ret[nrow(.)] < portfolio.returns[length(portfolio.returns)]

        monthly_win_vec <- c(monthly_win_vec, monthly_win_yn)

        # Risk Ratio
        risk_ratio_vec <- c(risk_ratio_vec, portfolio.returns[length(portfolio.returns)])

        # 누적 수익률 저장
        if (nrow(rets_cum) == 0) {
          rets_cum <- rbind(rets_cum, data.frame(date= rets_cum_temp$date, return = portfolio.returns))
        } else {
          rets_cum <- rbind(rets_cum, data.frame(date= rets_cum_temp$date[-1],
                                                 return = ((1+portfolio.returns[-1])*(1+rets_cum$return[nrow(rets_cum)])-1)))
        }
      }

      mdd_vec_wo_corona <- daily_rets_df[as.Date(names(daily_rets_df)) < '2020-03-01' | as.Date(names(daily_rets_df)) > '2020-04-15' ]

      model_nm_temp =
        paste0(
          str_pad(l, side='left', width=2, pad='0'), ".",
          pred_col[l], ", ",
          "Top", top_n[l], ", ",
          "SN: ", format(SN_ratio[l], nsmall = 1),
          ifelse(upper_bound[l]==999999 & lower_bound[l]==-999999, "", paste0(", Sell Bound: (", ifelse(lower_bound[l]==-999999, "None", lower_bound[l]), ", ", ifelse(upper_bound[l]==999999, "None", upper_bound[l]), ")")),
          ifelse(is.na(seed_money[l]) == FALSE, paste0(", Seed: ", seed_money[l]/1000000, "M  ["), "  ["),
          "Win Ratio: ", round(sum(monthly_win_vec) / length(monthly_win_vec), 2), ", ",
          "Hit Ratio: ", round(sum(risk_ratio_vec > 0) / length(risk_ratio_vec), 2), ", ",
          "Stability: ", round(mean(risk_ratio_vec) / sd(risk_ratio_vec), 2), ", ",
          "MDD: ", round(maxDrawdown(xts(mdd_vec_wo_corona, order.by=as.Date(names(mdd_vec_wo_corona)))), 2), "] ",
          "Return: ", rets_cum %>% filter(date == max(date)) %>% pull(return) %>% round(2)
        )

      rets_save <- rbind(rets_save, rets_cum %>% mutate(model_nm = model_nm_temp))
      print(model_nm_temp)
    }

    rets_save <- rbind(rets_save,
                       d_kospi_kosdaq_cum %>% select(date, return=kospi_cumret) %>% mutate(model_nm = "KOSPI") %>% filter(date <= max(rets_save$date)),
                       d_kospi_kosdaq_cum %>% select(date, return=kosdaq_cumret) %>% mutate(model_nm = "KOSDAQ") %>% filter(date <= max(rets_save$date)))

    toc()

    rets_save %>% mutate(label = if_else(date == max(date), as.character(round(return,2)), NA_character_)) %>%
      ggplot(aes(x=ymd(date), y=return, col=model_nm)) +
      geom_line(size=1.1) +
      #theme_set(bigstatsr::theme_bigstatsr()) +
      ggtitle("Portfolio Return") +
      theme(legend.position = c(0.01, 0.99),
            legend.justification = c(0, 1),
            legend.text=element_text(size=8),
            legend.title=element_blank(),
            legend.key = element_rect(fill="transparent", colour = "transparent"),
            legend.background = element_rect(fill="transparent", colour = "transparent")
      ) +
      #scale_color_brewer(palette = "Set1") +
      scale_x_date(date_labels="%Y-%m",date_breaks  ="6 month") +
      xlab("Date") +
      ylab("Cumulative Return") +
      ylim(min(rets_save$return), max(rets_save$return)+(max(rets_save$return)*0.2)) +
      labs(caption = paste0(min(rets_save$date), ' / ', max(rets_save$date))) +
      geom_label_repel(aes(label = label),
                       nudge_x = 1,
                       na.rm = TRUE,
                       show.legend = FALSE) +
      ggtitle(test_title)
  }
