rm(list=ls())
library('dplyr')
library('tidyr')
library('readr')
library('ggplot2')

theme_set(theme_minimal())

ctypes <- cols(
    Brandname = col_factor(),
    Everbroke = col_logical(),
    Hoursused.Band = col_factor(),
    Individual.ID = col_integer(),
    Number.of.Records = col_integer(),
    Prior.vs.Current.TV = col_factor(),
    Surveyyear = col_integer(),
    Tv.Cost.Label = col_factor(),
    Tv.Experience.Label = col_factor(),
    TV.PROBLEMS.10.SlowInternet = col_logical(),
    TV.PROBLEMS.11.SlowFunction = col_logical(),
    TV.PROBLEMS.12.SmartTVapps = col_logical(),
    TV.PROBLEMS.13.Soundproblems = col_logical(),
    TV.PROBLEMS.14.NoPowerOn = col_logical(),
    TV.PROBLEMS.15.TvTurnsOff = col_logical(),
    TV.PROBLEMS.16.OtherProb = col_logical(),
    TV.PROBLEMS.16.OtherProb.Writein = col_logical(),
    TV.PROBLEMS.17.NoProblems = col_logical(),
    TV.PROBLEMS.18.Dontrecall = col_logical(),
    TV.PROBLEMS.18.FailedPowerOn = col_logical(),
    TV.PROBLEMS.19.LosesPictureIntermit = col_logical(),
    TV.PROBLEMS.1.CableConnection = col_logical(),
    TV.PROBLEMS.20.LostPicture = col_logical(),
    TV.PROBLEMS.21.LosesSoundIntermit = col_logical(),
    TV.PROBLEMS.22.LostSound = col_logical(),
    TV.PROBLEMS.23.DeadPixels = col_logical(),
    TV.PROBLEMS.24.Discoloration = col_logical(),
    TV.PROBLEMS.25.HDMIInputs = col_logical(),
    TV.PROBLEMS.26.RemoteControl = col_logical(),
    TV.PROBLEMS.27.SmartAppsFroze = col_logical(),
    TV.PROBLEMS.28.TurnedOff = col_logical(),
    TV.PROBLEMS.29.BurnedGraphic = col_logical(),
    TV.PROBLEMS.2.DeadPixels = col_logical(),
    TV.PROBLEMS.30.WifiConnection = col_logical(),
    TV.PROBLEMS.3.PictureQuality = col_logical(),
    TV.PROBLEMS.4.Internet = col_logical(),
    TV.PROBLEMS.5.HDMIinput = col_logical(),
    TV.PROBLEMS.6.LostPicture = col_logical(),
    TV.PROBLEMS.7.3Dproblems = col_logical(),
    TV.PROBLEMS.8.RemoteControl = col_logical(),
    Tv.Problems.97 = col_logical(),
    TV.PROBLEMS.9.ScreenCracked = col_logical(),
    TV.RANK.1.Performance = col_integer(),
    TV.RANK.2.Reliability = col_integer(),
    TV.RANK.3.Price = col_integer(),
    Tv.Recommend.Label = col_factor(),
    Tv.Reliable.Label = col_factor(),
    Tv.Retailer.998.Other = col_character(),
    TV.SATRATE.10.SelectionVideoStreaming.LABEL = col_factor(),
    TV.SATRATE.11.AccessPhotos.LABEL = col_factor(),
    TV.SATRATE.12.HDMIInputs.LABEL = col_factor(),
    TV.SATRATE.13.USBPorts.LABEL = col_factor(),
    TV.SATRATE.14.VideoGame.LABEL = col_factor(),
    TV.SATRATE.15.CompatabilityAssistant.LABEL = col_factor(),
    TV.SATRATE.1.BrandReputation.LABEL = col_factor(),
    TV.SATRATE.2.Appearance.LABEL = col_factor(),
    TV.SATRATE.3.PictureQuality.LABEL = col_factor(),
    TV.SATRATE.4.Sound.Quality.LABEL = col_factor(),
    TV.SATRATE.5.AccessUSB.LABEL = col_factor(),
    TV.SATRATE.6.ConnectingInternet.LABEL = col_factor(),
    TV.SATRATE.7.MenuNavigation.LABEL = col_factor(),
    TV.SATRATE.8.ScreenSize.LABEL = col_factor(),
    TV.SATRATE.9.SelectionMusicStreaming.LABEL = col_factor(),
    Tv.Screen.Size.Label = col_factor(),
    Tv.Type.Label = col_factor(),
    Year.Purchased = col_integer(),
    Years.Owned = col_integer()
)

data <- read_csv('./data/tv_test.csv',
                 col_names = names(ctypes$cols),
                 col_types = ctypes,
                 skip=1
            )


data <- data %>%
    mutate_at(vars(starts_with("TV.PROBLEM")), list( ~if_else( is.na(.), FALSE, . )))

data <- data %>%
    mutate(Tv.Retailer.998.Other = Tv.Retailer.998.Other %>% toupper()) %>%
    mutate(AnyProblems = TV.PROBLEMS.10.SlowInternet |
               TV.PROBLEMS.11.SlowFunction |
               TV.PROBLEMS.12.SmartTVapps |
               TV.PROBLEMS.13.Soundproblems |
               TV.PROBLEMS.14.NoPowerOn |
               TV.PROBLEMS.15.TvTurnsOff |
               TV.PROBLEMS.16.OtherProb |
               TV.PROBLEMS.16.OtherProb.Writein |
               TV.PROBLEMS.17.NoProblems |
               TV.PROBLEMS.18.Dontrecall |
               TV.PROBLEMS.18.FailedPowerOn |
               TV.PROBLEMS.19.LosesPictureIntermit |
               TV.PROBLEMS.1.CableConnection |
               TV.PROBLEMS.20.LostPicture |
               TV.PROBLEMS.21.LosesSoundIntermit |
               TV.PROBLEMS.22.LostSound |
               TV.PROBLEMS.23.DeadPixels |
               TV.PROBLEMS.24.Discoloration |
               TV.PROBLEMS.25.HDMIInputs |
               TV.PROBLEMS.26.RemoteControl |
               TV.PROBLEMS.27.SmartAppsFroze |
               TV.PROBLEMS.28.TurnedOff |
               TV.PROBLEMS.29.BurnedGraphic |
               TV.PROBLEMS.2.DeadPixels |
               TV.PROBLEMS.30.WifiConnection |
               TV.PROBLEMS.3.PictureQuality |
               TV.PROBLEMS.4.Internet |
               TV.PROBLEMS.5.HDMIinput |
               TV.PROBLEMS.6.LostPicture |
               TV.PROBLEMS.7.3Dproblems |
               TV.PROBLEMS.8.RemoteControl) %>%
    mutate(Tv.Recommend.Label = factor(Tv.Recommend.Label, levels = c("Extremely likely", "Somewhat likely", "Not very likely", "Not at all likely"))) %>%
    mutate(Tv.Experience.Label = factor(Tv.Experience.Label, labels = c("Worked perfectly", "Minor problems", "Major Problems", "Unknown"))) %>%
    mutate(Tv.Reliable.Label = factor(Tv.Reliable.Label, labels = c("Very reliable", "Very problematic", "Unknown", "Somewhat reliable", "Somewhat problematic"))) %>%
    mutate(Tv.Reliable.Label = factor(Tv.Reliable.Label, levels = c("Very reliable", "Somewhat reliable", "Somewhat problematic", "Very problematic", "Unknown")))

#View(data)

# brands.counts <- data %>% count(Brandname) %>%
#     transmute(Count = n()) %>%
#     mutate(MktShare = Count / sum(Count)) %>%
#     arrange(desc(Count))
brands.counts <- data %>%
    group_by(Brandname) %>%
    summarise(Count = n()) %>%
    mutate(MktShare = Count / sum(Count), Rank = min_rank(-Count)) %>%
    ungroup() %>%
    select(Brandname, Count, MktShare, Rank) %>%
    arrange(Rank)


'Brand Counts' %>% print()
brands.counts %>% print()

# LG year trend
lg.year.trends <- data %>%
    filter(Brandname == "LG" & !is.na(Year.Purchased)) %>%
    group_by(Year.Purchased) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    arrange(Year.Purchased)

'LG Year Trend' %>% print()
lg.year.trends %>% print()

# LG Problem rate overall
prob.rates <- data %>% group_by(Brandname) %>%
    summarise(AnyProblemRate = mean(AnyProblems)) %>%
    mutate(Rank = min_rank(AnyProblemRate)) %>%
    ungroup() %>%
    arrange(AnyProblemRate)

'Problem rate by brand' %>% print
prob.rates %>% print()

# LG Experience
perfect.rates <- data %>%
    mutate(perf_exp = Tv.Experience.Label == "Worked perfectly") %>%
    filter(!is.na(perf_exp)) %>%
    group_by(Brandname) %>%
    summarise(PerfectExperienceRate = mean(perf_exp)) %>%
    mutate(Rank = min_rank(-PerfectExperienceRate)) %>%
    arrange(desc(PerfectExperienceRate))

'Perfect experience rate by brand' %>% print()
print(perfect.rates)

# LG Reccomend rate
reccomend.rates <- data %>%
    mutate(rec_rate = Tv.Recommend.Label %in% c("Extremely likely",  "Somewhat likely")) %>%
    filter(!is.na(rec_rate)) %>%
    group_by(Brandname) %>%
    summarise(ReccomendRate = mean(rec_rate)) %>%
    mutate(Rank = min_rank(-ReccomendRate)) %>%
    arrange(desc(ReccomendRate))

'Reccomendation rate by brand' %>% print()
print(reccomend.rates)

# LG reccomend vs.
## Year Purchased
lg.recc.year.trends <- data %>%
    filter(Brandname == "LG" & !is.na(Year.Purchased) & !is.na(Tv.Recommend.Label)) %>%
    group_by(Year.Purchased, Tv.Recommend.Label) %>%
    summarise(Count = n()) %>%
    mutate(pct = (Count) / sum(Count)) %>%
    ungroup() %>%
    arrange(Year.Purchased, Tv.Recommend.Label)

'Reccomendation rated by year purchased (LG)' %>% print()
lg.recc.year.trends %>% print()

## Tv Type
### Reccomendation
lg.tv.type.recc <- data %>%
    filter(Brandname == "LG" & !is.na(Tv.Type.Label) & !is.na(Tv.Recommend.Label)) %>%
    group_by(Tv.Type.Label, Tv.Recommend.Label) %>%
    summarise(Count = n()) %>%
    mutate(pct = (Count) / sum(Count)) %>%
    ungroup() %>%
    arrange(Tv.Type.Label, Tv.Recommend.Label)

'Reccomendation by TV type (LG)' %>% print()
lg.tv.type.recc %>% print()

### Reliability
lg.tv.type.reliable <- data %>%
    filter(Brandname == "LG" & !is.na(Tv.Type.Label) & !is.na(Tv.Reliable.Label) & Tv.Reliable.Label != 5) %>%
    group_by(Tv.Type.Label, Tv.Reliable.Label) %>%
    summarise(Count = n() ) %>%
    mutate(pct = (Count) / sum(Count)) %>%
    ungroup() %>%
    arrange(Tv.Type.Label, Tv.Reliable.Label)

'Reliability by TV type (LG)' %>% print()
lg.tv.type.reliable %>% print()

# LG alone, vs. all, and vs. samsumg for:
## Experience
# print(perfect.rates)
exp.rates <- data %>%
    filter(!is.na(Tv.Experience.Label) & Tv.Experience.Label != "Unknown") %>%
    group_by(Brandname, Tv.Experience.Label) %>%
    summarise(Count = n()) %>%
    mutate(pct = (Count) / sum(Count)) %>%
    ungroup() %>%
    arrange(desc(Brandname, Tv.Experience.Label))

exp.rates.lg <- exp.rates %>% filter(Brandname == 'LG')
exp.rates.sam <- exp.rates %>% filter(Brandname == 'SAMSUNG')
exp.rates.all <- exp.rates %>%
    group_by(Tv.Experience.Label) %>%
    summarise(Count = sum(Count)) %>%
    mutate(pct = Count / sum(Count)) %>%
    mutate(Brandname = 'ALL') %>%
    select(Brandname, Tv.Experience.Label, Count, pct) %>%
    ungroup() %>%
    arrange(desc(Brandname), Tv.Experience.Label)

# exp.rates.lg %>% print()
# exp.rates.sam %>% print()
# exp.rates.all %>% print()

exp.rates.comp <- exp.rates.lg %>%
    left_join(exp.rates.sam, by="Tv.Experience.Label", suffix = c('', '.sam')) %>%
    left_join(exp.rates.all, by="Tv.Experience.Label", suffix = c('', '.all')) %>%
    mutate(LGvSamsung = pct / pct.sam) %>%
    mutate(LGvAll = pct / pct.all) %>%
    select(Tv.Experience.Label, Count, pct, pct.sam, pct.all, LGvSamsung, LGvAll)

'Experience comparison: LG v Samsung v All' %>% print()
exp.rates.comp %>% print()

## Reccomend
rec.rates <- data %>%
    filter(!is.na(Tv.Recommend.Label)) %>%
    group_by(Brandname, Tv.Recommend.Label) %>%
    summarise(Count = n()) %>%
    mutate(pct = (Count) / sum(Count)) %>%
    ungroup() %>%
    arrange(desc(Brandname, Tv.Recommend.Label))

rec.rates.lg <- rec.rates %>% filter(Brandname == 'LG')
rec.rates.sam <- rec.rates %>% filter(Brandname == 'SAMSUNG')
rec.rates.all <- rec.rates %>%
    group_by(Tv.Recommend.Label) %>%
    summarise(Count = sum(Count)) %>%
    mutate(pct = Count / sum(Count)) %>%
    mutate(Brandname = 'ALL') %>%
    select(Brandname, Tv.Recommend.Label, Count, pct) %>%
    ungroup() %>%
    arrange(desc(Brandname), Tv.Recommend.Label)

# rec.rates.lg %>% print()
# rec.rates.sam %>% print()
# rec.rates.all %>% print()

rec.rates.comp <- rec.rates.lg %>%
    left_join(rec.rates.sam, by="Tv.Recommend.Label", suffix = c('', '.sam')) %>%
    left_join(rec.rates.all, by="Tv.Recommend.Label", suffix = c('', '.all')) %>%
    mutate(LGvSamsung = pct / pct.sam) %>%
    mutate(LGvAll = pct / pct.all) %>%
    select(Tv.Recommend.Label, Count, pct, pct.sam, pct.all, LGvSamsung, LGvAll)


'Reccomendation comparison: LG v Samsung v All' %>% print()
rec.rates.comp %>% print()

## Reliablility
rel.rates <- data %>%
    filter(!is.na(Tv.Reliable.Label) & Tv.Reliable.Label != "Unknown") %>%
    group_by(Brandname, Tv.Reliable.Label) %>%
    summarise(Count = n()) %>%
    mutate(pct = (Count) / sum(Count)) %>%
    ungroup() %>%
    arrange(desc(Brandname, Tv.Reliable.Label))

rel.rates.lg <- rel.rates %>% filter(Brandname == 'LG')
rel.rates.sam <- rel.rates %>% filter(Brandname == 'SAMSUNG')
rel.rates.all <- rel.rates %>%
    group_by(Tv.Reliable.Label) %>%
    summarise(Count = sum(Count)) %>%
    mutate(pct = Count / sum(Count)) %>%
    mutate(Brandname = 'ALL') %>%
    select(Brandname, Tv.Reliable.Label, Count, pct) %>%
    ungroup() %>%
    arrange(desc(Brandname), Tv.Reliable.Label)

# rel.rates.lg %>% print()
# rel.rates.sam %>% print()
# rel.rates.all %>% print()

rel.rates.comp <- rel.rates.lg %>%
    left_join(rel.rates.sam, by="Tv.Reliable.Label", suffix = c('', '.sam')) %>%
    left_join(rel.rates.all, by="Tv.Reliable.Label", suffix = c('', '.all')) %>%
    mutate(LGvSamsung = pct / pct.sam) %>%
    mutate(LGvAll = pct / pct.all) %>%
    select(Tv.Reliable.Label, Count, pct, pct.sam, pct.all, LGvSamsung, LGvAll)


'Reliability comparison: LG v Samsung v All' %>% print()
rel.rates.comp %>% print()

## Problems
problems <- data %>%
    group_by(Brandname) %>%
    summarise(
        SlowInternetRate = mean(TV.PROBLEMS.10.SlowInternet),
        SlowFunctionRate = mean(TV.PROBLEMS.11.SlowFunction),
        SmartTVappsRate = mean(TV.PROBLEMS.12.SmartTVapps),
        SoundproblemsRate = mean(TV.PROBLEMS.13.Soundproblems),
        NoPowerOnRate = mean(TV.PROBLEMS.14.NoPowerOn),
        TvTurnsOffRate = mean(TV.PROBLEMS.15.TvTurnsOff),
        OtherProbRate = mean(TV.PROBLEMS.16.OtherProb),
        OtherProbRate = mean(TV.PROBLEMS.16.OtherProb),
        NoProblemsRate = mean(TV.PROBLEMS.17.NoProblems),
        DontrecallRate = mean(TV.PROBLEMS.18.Dontrecall),
        FailedPowerOnRate = mean(TV.PROBLEMS.18.FailedPowerOn),
        LosesPictureIntermitRate = mean(TV.PROBLEMS.19.LosesPictureIntermit),
        CableConnectionRate = mean(TV.PROBLEMS.1.CableConnection),
        LostPictureRate = mean(TV.PROBLEMS.20.LostPicture),
        LosesSoundIntermitRate = mean(TV.PROBLEMS.21.LosesSoundIntermit),
        LostSoundRate = mean(TV.PROBLEMS.22.LostSound),
        DeadPixelsRate = mean(TV.PROBLEMS.23.DeadPixels),
        DiscolorationRate = mean(TV.PROBLEMS.24.Discoloration),
        HDMIInputsRate = mean(TV.PROBLEMS.25.HDMIInputs),
        RemoteControlRate = mean(TV.PROBLEMS.26.RemoteControl),
        SmartAppsFrozeRate = mean(TV.PROBLEMS.27.SmartAppsFroze),
        TurnedOffRate = mean(TV.PROBLEMS.28.TurnedOff),
        BurnedGraphicRate = mean(TV.PROBLEMS.29.BurnedGraphic),
        DeadPixelsRate = mean(TV.PROBLEMS.2.DeadPixels),
        WifiConnectionRate = mean(TV.PROBLEMS.30.WifiConnection),
        PictureQualityRate = mean(TV.PROBLEMS.3.PictureQuality),
        InternetRate = mean(TV.PROBLEMS.4.Internet),
        HDMIinputRate = mean(TV.PROBLEMS.5.HDMIinput),
        LostPictureRate = mean(TV.PROBLEMS.6.LostPicture),
        ThreeDproblemsRate = mean(TV.PROBLEMS.7.3Dproblems),
        RemoteControlRate = mean(TV.PROBLEMS.8.RemoteControl)
    ) %>% ungroup() %>%
    gather("Problem", "IncidenceRate", -Brandname) %>%
    filter(IncidenceRate != 0 ) %>%
    spread(Problem, IncidenceRate, fill = 0)

problems.T <- problems %>% gather('Category', 'Rate', -Brandname) %>% spread(Brandname, Rate)

'Problem rates by brand' %>% print()
problems.T %>% print()

problems.rank <- problems %>% mutate(
        BurnedGraphicRank = min_rank(BurnedGraphicRate),
        DiscolorationRank = min_rank(DiscolorationRate),
        FailedPowerOnRank = min_rank(FailedPowerOnRate),
        HDMIInputsRank = min_rank(HDMIInputsRate),
        LosesPictureIntermitRank = min_rank(LosesPictureIntermitRate),
        LosesSoundIntermitRank = min_rank(LosesSoundIntermitRate),
        LostSoundRank = min_rank(LostSoundRate),
        SmartAppsFrozeRank = min_rank(SmartAppsFrozeRate),
        TurnedOffRank = min_rank(TurnedOffRate),
        WifiConnectionRank = min_rank(WifiConnectionRate)
    ) %>%
    mutate(AvgRank = rowMeans(select(., -Brandname))) %>%
    mutate(OverallRank = min_rank(AvgRank)) %>%
    select(Brandname, OverallRank, BurnedGraphicRank:WifiConnectionRank)

problems.rank.T <- problems.rank %>% gather('Category', 'Rate', -Brandname) %>% spread(Brandname, Rate) %>% arrange(Category=='OverallRank', LG, Category)

'Problem rates ranking by brand' %>% print()
problems.rank.T %>% print()



## Satisfaction
#"Excellent"             "Very good"             "Good"                  "Unsure/Not applicable" "Fair"                  "Very poor"             "Poor"
satisfaction <- data %>%
    group_by(Brandname) %>%
    summarise(
        SelectionVideoStreamingSatRate
        = mean(TV.SATRATE.10.SelectionVideoStreaming.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        AccessPhotosSatRate
        = mean(TV.SATRATE.11.AccessPhotos.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        HDMIInputsSatRate
        = mean(TV.SATRATE.12.HDMIInputs.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        USBPortsSatRate
        = mean(TV.SATRATE.13.USBPorts.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        VideoGameSatRate
        = mean(TV.SATRATE.14.VideoGame.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        CompatabilityAssistantSatRate
        = mean(TV.SATRATE.15.CompatabilityAssistant.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        BrandReputationSatRate
        = mean(TV.SATRATE.1.BrandReputation.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        AppearanceSatRate
        = mean(TV.SATRATE.2.Appearance.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        PictureQualitySatRate
        = mean(TV.SATRATE.3.PictureQuality.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        SoundSatRate =
            mean(TV.SATRATE.4.Sound.Quality.LABEL
                 %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        AccessUSBSatRate
        = mean(TV.SATRATE.5.AccessUSB.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        ConnectingInternetSatRate
        = mean(TV.SATRATE.6.ConnectingInternet.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        MenuNavigationSatRate
        = mean(TV.SATRATE.7.MenuNavigation.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        ScreenSizeSatRate
        = mean(TV.SATRATE.8.ScreenSize.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable")),
        SelectionMusicStreamingSatRate
        = mean(TV.SATRATE.9.SelectionMusicStreaming.LABEL
               %in% c("Excellent", "Very good", "Good", "Unsure/Not applicable"))
    ) %>% ungroup()
satisfaction.T <- satisfaction %>% gather('Category', 'Rate', -Brandname) %>% spread(Brandname, Rate)

'Satisfaction rates by brand' %>% print()
satisfaction.T %>% print()

satisfaction.rank <- satisfaction %>%
    mutate(
        SelectionVideoStreamingSatRank = min_rank(desc(SelectionVideoStreamingSatRate)),
        AccessPhotosSatRank = min_rank(desc(AccessPhotosSatRate)),
        HDMIInputsSatRank = min_rank(desc(HDMIInputsSatRate)),
        USBPortsSatRank = min_rank(desc(USBPortsSatRate)),
        VideoGameSatRank = min_rank(desc(VideoGameSatRate)),
        CompatabilityAssistantSatRank = min_rank(desc(CompatabilityAssistantSatRate)),
        BrandReputationSatRank = min_rank(desc(BrandReputationSatRate)),
        AppearanceSatRank = min_rank(desc(AppearanceSatRate)),
        PictureQualitySatRank = min_rank(desc(PictureQualitySatRate)),
        SoundSatRank = min_rank(desc(SoundSatRate)),
        AccessUSBSatRank = min_rank(desc(AccessUSBSatRate)),
        ConnectingInternetSatRank = min_rank(desc(ConnectingInternetSatRate)),
        MenuNavigationSatRank = min_rank(desc(MenuNavigationSatRate)),
        ScreenSizeSatRank = min_rank(desc(ScreenSizeSatRate)),
        SelectionMusicStreamingSatRank = min_rank(desc(SelectionMusicStreamingSatRate))
        ) %>%
    select(Brandname, SelectionVideoStreamingSatRank:SelectionMusicStreamingSatRank) %>%
    mutate(AvgRank = rowMeans(select(., -Brandname))) %>%
    mutate(OverallRank = min_rank(AvgRank)) %>%
    select(Brandname, OverallRank, SelectionVideoStreamingSatRank:SelectionMusicStreamingSatRank)

satisfaction.rank.T <- satisfaction.rank %>% gather('Category', 'Rate', -Brandname) %>% spread(Brandname, Rate) %>% arrange(Category=='OverallRank', LG, Category)

'Satisfaction rates ranking by brand' %>% print()
satisfaction.rank.T %>% print()

## Logistic Regression

data.lr <- data %>%
    mutate(
        WouldReccomend = Tv.Recommend.Label %in% c("Extremely likely",  "Somewhat likely"),
        SlowFunctionProblem = TV.PROBLEMS.11.SlowFunction %>% as.numeric(),
        SmartTVappsProblem = TV.PROBLEMS.12.SmartTVapps %>% as.numeric(),
        SoundproblemsProblem = TV.PROBLEMS.13.Soundproblems %>% as.numeric(),
        NoPowerOnProblem = TV.PROBLEMS.14.NoPowerOn %>% as.numeric(),
        TvTurnsOffProblem = TV.PROBLEMS.15.TvTurnsOff %>% as.numeric(),
        OtherProbProblem = TV.PROBLEMS.16.OtherProb %>% as.numeric(),
        OtherProbProblem = TV.PROBLEMS.16.OtherProb %>% as.numeric(),
        NoProblemsProblem = TV.PROBLEMS.17.NoProblems %>% as.numeric(),
        DontrecallProblem = TV.PROBLEMS.18.Dontrecall %>% as.numeric(),
        FailedPowerOnProblem = TV.PROBLEMS.18.FailedPowerOn %>% as.numeric(),
        LosesPictureIntermitProblem = TV.PROBLEMS.19.LosesPictureIntermit %>% as.numeric(),
        CableConnectionProblem = TV.PROBLEMS.1.CableConnection %>% as.numeric(),
        LostPictureProblem = TV.PROBLEMS.20.LostPicture %>% as.numeric(),
        LosesSoundIntermitProblem = TV.PROBLEMS.21.LosesSoundIntermit %>% as.numeric(),
        LostSoundProblem = TV.PROBLEMS.22.LostSound %>% as.numeric(),
        DeadPixelsProblem = TV.PROBLEMS.23.DeadPixels %>% as.numeric(),
        DiscolorationProblem = TV.PROBLEMS.24.Discoloration %>% as.numeric(),
        HDMIInputsProblem = TV.PROBLEMS.25.HDMIInputs %>% as.numeric(),
        RemoteControlProblem = TV.PROBLEMS.26.RemoteControl %>% as.numeric(),
        SmartAppsFrozeProblem = TV.PROBLEMS.27.SmartAppsFroze %>% as.numeric(),
        TurnedOffProblem = TV.PROBLEMS.28.TurnedOff %>% as.numeric(),
        BurnedGraphicProblem = TV.PROBLEMS.29.BurnedGraphic %>% as.numeric(),
        DeadPixelsProblem = TV.PROBLEMS.2.DeadPixels %>% as.numeric(),
        WifiConnectionProblem = TV.PROBLEMS.30.WifiConnection %>% as.numeric(),
        PictureQualityProblem = TV.PROBLEMS.3.PictureQuality %>% as.numeric(),
        InternetProblem = TV.PROBLEMS.4.Internet %>% as.numeric(),
        HDMIinputProblem = TV.PROBLEMS.5.HDMIinput %>% as.numeric(),
        LostPictureProblem = TV.PROBLEMS.6.LostPicture %>% as.numeric(),
        ThreeDproblemsProblem = TV.PROBLEMS.7.3Dproblems %>% as.numeric(),
        RemoteControlProblem = TV.PROBLEMS.8.RemoteControl %>% as.numeric(),
        SelectionVideoStreamingSat = TV.SATRATE.10.SelectionVideoStreaming.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        AccessPhotosSat = TV.SATRATE.11.AccessPhotos.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        HDMIInputsSat = TV.SATRATE.12.HDMIInputs.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        USBPortsSat = TV.SATRATE.13.USBPorts.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        VideoGameSat = TV.SATRATE.14.VideoGame.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        CompatabilityAssistantSat = TV.SATRATE.15.CompatabilityAssistant.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        BrandReputationSat = TV.SATRATE.1.BrandReputation.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        AppearanceSat = TV.SATRATE.2.Appearance.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        PictureQualitySat = TV.SATRATE.3.PictureQuality.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        SoundSat = TV.SATRATE.4.Sound.Quality.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        AccessUSBSat = TV.SATRATE.5.AccessUSB.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        ConnectingInternetSat = TV.SATRATE.6.ConnectingInternet.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        MenuNavigationSat = TV.SATRATE.7.MenuNavigation.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        ScreenSizeSat = TV.SATRATE.8.ScreenSize.LABEL %in% c("Excellent", "Very good") %>% as.numeric(),
        SelectionMusicStreamingSat = TV.SATRATE.9.SelectionMusicStreaming.LABEL %in% c("Excellent", "Very good") %>% as.numeric()
    ) %>%
    select(WouldReccomend, BurnedGraphicProblem,  DiscolorationProblem,  FailedPowerOnProblem,  HDMIInputsProblem,  LosesPictureIntermitProblem,
           LosesSoundIntermitProblem,  LostSoundProblem,  SmartAppsFrozeProblem,  TurnedOffProblem,  WifiConnectionProblem,
           SelectionVideoStreamingSat, AccessPhotosSat, HDMIInputsSat, USBPortsSat, VideoGameSat, CompatabilityAssistantSat,
           BrandReputationSat, AppearanceSat, PictureQualitySat, SoundSat, AccessUSBSat, ConnectingInternetSat, MenuNavigationSat,
           ScreenSizeSat, SelectionMusicStreamingSat)

model <- glm(WouldReccomend ~ ., family=binomial, data=data.lr)
#model %>% summary() %>% print()
coefs <- (model %>% summary)$coefficients
signif.coefs <- coefs[coefs[,4]<1e-6, ]
signif.coefs <- signif.coefs[order(-abs(signif.coefs[, 1])), ]

'Significant facotors influencing liklihood to reccomend a product' %>% print()
signif.coefs %>% print()