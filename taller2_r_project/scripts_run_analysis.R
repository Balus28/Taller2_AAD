source("R/utils.R")

dir.create("output", showWarnings = FALSE)

single <- run_single_experiment(split_seed = 123, cv_seed_base = 1000)
repeated <- run_repeated_experiments(n_repeats = 10, split_seed_start = 5000, cv_seed_start = 9000)

write.csv(single$spline_cv$summary, "output/cv_regression_splines.csv", row.names = FALSE)
write.csv(single$function_cv$summary, "output/cv_function_basis_models.csv", row.names = FALSE)
write.csv(single$local_cv$summary, "output/cv_local_models.csv", row.names = FALSE)
write.csv(single$test_results, "output/test_results_single_split.csv", row.names = FALSE)
write.csv(repeated, "output/test_results_repeated.csv", row.names = FALSE)

png("output/test_mse_distributions.png", width = 1000, height = 650)
print(plot_test_mse_distributions(repeated))
dev.off()

saveRDS(single, "output/single_experiment_objects.rds")
saveRDS(repeated, "output/repeated_experiment_objects.rds")

cat("Analysis completed. Results saved in output/.\n")
