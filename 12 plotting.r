
plot_str(ZEUS_mcq300c)

library(DataExplorer)


pdf("histogram.pdf", width=40, height=15)
plot_histogram(ZEUS_mcq300c)

dev.off()

#corr
pdf("correlation.pdf", width=40, height=15)
plot_correlation(ZEUS_mcq300c, type = 'continuous','Review.Date')
dev.off()

#bar plot catg
pdf("bar_plot_catg.pdf", width=40, height=15)
plot_bar(ZEUS_mcq300c)
dev.off()

