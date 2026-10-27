###############################################################################
# WAR (card game) -- Monte Carlo simulation
#
# Rules implemented
#   * Standard 52-card deck (ranks 2..14, Ace high), split evenly between 2 players.
#   * Optional: add 2 Jokers (27 cards each). A Joker is a wild "tie-maker":
#     any flip involving a Joker automatically triggers a war, no matter what
#     the opposing card is (including another Joker).
#   * On a tie, each player puts `facedown` cards face down, then flips one
#     card face up. Higher wins the whole pot. Ties/Jokers repeat the war.
#   * If a player can't cover the full face-down count, they put down as many
#     as they can while keeping one card for the face-up flip. If a player has
#     no cards left when a war starts, they lose.
#   * Winnings go to the bottom of the winner's hand, in random order by
#     default (this prevents the endless loops that fixed order can produce).
#
# "Turn" = one battle (the initial flip plus any chain of wars that follows).
# Set war_counts_as_turn = TRUE if you'd rather count every face-up flip round.
###############################################################################

JOKER <- 15L  # internal code for a Joker

# ---- Play a single game ------------------------------------------------------
play_war <- function(facedown = 3,
                     jokers = FALSE,
                     max_turns = 20000L,
                     shuffle_winnings = TRUE,
                     war_counts_as_turn = FALSE) {

  deck <- rep(2:14, each = 4)
  if (jokers) deck <- c(deck, JOKER, JOKER)
  deck <- deck[sample.int(length(deck))]
  half <- length(deck) %/% 2
  a <- deck[seq_len(half)]
  b <- deck[(half + 1):length(deck)]

  gather <- function(pot) {
    if (shuffle_winnings) pot[sample.int(length(pot))] else pot
  }

  turns <- 0L
  wars  <- 0L

  while (length(a) > 0L && length(b) > 0L && turns < max_turns) {
    turns <- turns + 1L

    ca <- a[1]; a <- a[-1]
    cb <- b[1]; b <- b[-1]
    pot <- c(ca, cb)

    repeat {
      tie <- (ca == JOKER) || (cb == JOKER) || (ca == cb)

      if (!tie) {
        if (ca > cb) a <- c(a, gather(pot)) else b <- c(b, gather(pot))
        break
      }

      # ---- WAR ----
      wars <- wars + 1L
      if (war_counts_as_turn) turns <- turns + 1L

      # A player with no cards cannot continue and loses the war
      if (length(a) == 0L || length(b) == 0L) {
        if (length(a) > 0L) a <- c(a, gather(pot))
        else if (length(b) > 0L) b <- c(b, gather(pot))
        # (both empty = draw; nothing to distribute)
        break
      }

      # Face-down cards (keep at least one card for the face-up flip)
      da <- min(facedown, length(a) - 1L)
      db <- min(facedown, length(b) - 1L)
      pot <- c(pot, a[seq_len(da)], b[seq_len(db)])
      a <- a[seq_along(a) > da]
      b <- b[seq_along(b) > db]

      # Face-up cards
      ca <- a[1]; a <- a[-1]
      cb <- b[1]; b <- b[-1]
      pot <- c(pot, ca, cb)
    }
  }

  winner <- if (length(a) > 0L && length(b) == 0L) 1
            else if (length(b) > 0L && length(a) == 0L) 2
            else if (length(a) == 0L && length(b) == 0L) 0   # draw
            else NA_real_                                    # hit max_turns

  c(turns = turns, wars = wars, winner = winner)
}

# ---- Monte Carlo driver --------------------------------------------------------
# facedown : a number or a vector of numbers (e.g. c(0, 1, 3, 5)) to compare
# jokers   : FALSE, TRUE, or c(FALSE, TRUE) to compare both
simulate_war <- function(n_sims = 5000,
                         facedown = c(1, 3),
                         jokers = c(FALSE, TRUE),
                         seed = NULL,
                         plot = TRUE,
                         ...) {

  if (!is.null(seed)) set.seed(seed)

  settings <- expand.grid(facedown = facedown, jokers = jokers)
  all_results <- vector("list", nrow(settings))
  summaries   <- vector("list", nrow(settings))

  for (i in seq_len(nrow(settings))) {
    fd <- settings$facedown[i]
    jk <- settings$jokers[i]

    res <- t(vapply(seq_len(n_sims),
                    function(k) play_war(facedown = fd, jokers = jk, ...),
                    numeric(3)))
    res <- as.data.frame(res)
    done <- !is.na(res$winner)
    t_done <- res$turns[done]

    all_results[[i]] <- res
    summaries[[i]] <- data.frame(
      facedown   = fd,
      jokers     = jk,
      n_sims     = n_sims,
      mean_turns = mean(t_done),
      median     = median(t_done),
      sd         = sd(t_done),
      min        = min(t_done),
      p95        = unname(quantile(t_done, 0.95)),
      max        = max(t_done),
      mean_wars  = mean(res$wars[done]),
      p1_win_pct = 100 * mean(res$winner[done] == 1),
      draw_pct   = 100 * mean(res$winner[done] == 0),
      unfinished = sum(!done)
    )
  }

  summary_tbl <- do.call(rbind, summaries)
  rownames(summary_tbl) <- NULL

  cat("\n===== WAR SIMULATION RESULTS (", n_sims, " games per setting) =====\n", sep = "")
  print(format(summary_tbl, digits = 4), row.names = FALSE)
  cat("\nTurn = one battle incl. any chain of wars. Stats use finished games only.\n")

  if (plot) {
    all_turns <- unlist(lapply(all_results, function(r) r$turns[!is.na(r$winner)]))
    brks <- pretty(range(all_turns), n = 35)
    ymax <- max(vapply(all_results, function(r) {
      max(hist(r$turns[!is.na(r$winner)], breaks = brks, plot = FALSE)$counts)
    }, numeric(1)))

    nf <- length(unique(settings$facedown))
    nj <- length(unique(settings$jokers))
    op <- par(mfrow = c(nj, nf), mar = c(4, 4, 3, 1))
    on.exit(par(op), add = TRUE)

    for (i in seq_len(nrow(settings))) {
      t_done <- all_results[[i]]$turns[!is.na(all_results[[i]]$winner)]
      hist(t_done, breaks = brks, ylim = c(0, ymax * 1.05),
           col = "steelblue", border = "white",
           main = sprintf("Face-down: %d | Jokers: %s",
                          settings$facedown[i],
                          ifelse(settings$jokers[i], "Yes", "No")),
           xlab = "Turns to finish", ylab = "Games")
      abline(v = mean(t_done),   col = "red",      lwd = 2)
      abline(v = median(t_done), col = "darkgreen", lwd = 2, lty = 2)
      legend("topright", bty = "n", cex = 0.8,
             legend = c(sprintf("Mean = %.1f", mean(t_done)),
                        sprintf("Median = %.1f", median(t_done))),
             col = c("red", "darkgreen"), lty = c(1, 2), lwd = 2)
    }
  }

  invisible(list(summary = cbind(summary_tbl), settings = settings,
                 results = all_results))
}

# ---- Example usage -------------------------------------------------------------
# Compare 0, 1, 3 and 5 face-down cards, with and without Jokers:
out <- simulate_war(n_sims = 2000,
                    facedown = c(0, 1, 3, 5),
                    jokers = c(FALSE, TRUE),
                    seed = 42)

# Single configuration (standard deck, 3 face-down, 10,000 games):
# out <- simulate_war(n_sims = 10000, facedown = 3, jokers = FALSE)

# Count every war flip round as its own turn:
# out <- simulate_war(n_sims = 2000, facedown = 3, war_counts_as_turn = TRUE)

# Raw per-game data for a setting (turns, wars, winner):
# head(out$results[[1]])
