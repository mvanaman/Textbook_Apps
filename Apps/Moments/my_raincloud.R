require(gghalves)
position_jitternudge <- # ----
    function(jitter.width = NULL,
             jitter.height = 0,
             nudge.x = 0,
             nudge.y = 0,
             seed = NA) {
        if (!is.null(seed) && is.na(seed)) {
            seed <- sample.int(.Machine$integer.max, 1L)
        }
        
        ggplot2::ggproto(
            NULL,
            PositionJitternudge,
            jitter.width = jitter.width,
            jitter.height = jitter.height,
            nudge.x = nudge.x,
            nudge.y = nudge.y,
            seed = seed
        )
    }

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
 PositionJitternudge <-
    ggplot2::ggproto(
        "PositionJitternudge",
        ggplot2::Position,
        jitter.width = NULL,
        jitter.height = NULL,
        nudge.x = NULL,
        nudge.y = NULL,
        
        required_aes = c("x", "y"),
        
        setup_params = function(self, data) {
            flipped_aes <- ggplot2::has_flipped_aes(data)
            data <-
                ggplot2::flip_data(data, flipped_aes)
            width <-
                self$jitter.width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4)
            
            list(
                nudge.x = self$nudge.x,
                nudge.y = self$nudge.y,
                jitter.height = self$jitter.height,
                jitter.width = width / 2,
                #(ndodge + 2),
                seed = self$seed,
                flipped_aes = flipped_aes
            )
        },
        
        compute_panel = function(data, params, scales) {
            data <- ggplot2::flip_data(data, params$flipped_aes)
            
            trans_x <-
                if (params$jitter.width > 0)
                    function(x) {
                        jitter(x, amount = params$jitter.width) + params$nudge.x
                    }
            trans_y <-
                if (params$jitter.height > 0)
                    function(x) {
                        jitter(x, amount = params$jitter.height)  + params$nudge.y
                    }
            
            data <-
                ggplot2:::with_seed_null(params$seed,
                                         ggplot2::transform_position(data, trans_x, trans_y))
            ggplot2::flip_data(data, params$flipped_aes)
        }
    )
# raincloud ----
 
my_raincloud <- function (
    data, # data must be df with one variable 
    color = "#899DA4",
    fill = "#C93312",
    alpha = 0.6
) {
    # format data
    set.seed(352)
    colnames(data) <- c("y_axis") 
    # raincloud plot code ----
    
    figure_1x1_vertical <- 
        ggplot(data = data, aes(x = 1, y = y_axis)) + 
        geom_point(
            aes(color = "blue"),
            show.legend = FALSE,
            position = position_jitternudge(
                jitter.width = 0.1, 
                jitter.height = 0.05, 
                nudge.x = 0.28, 
                nudge.y = 0, 
                seed = 352
                ),
            alpha = 0.4,
            size = 0.5
        ) +
        geom_half_violin(
            fill = fill,
            color = color,
            alpha = alpha,
            position = position_nudge(x = 0.2),
            side = "l",
            show.legend = FALSE
        ) +
        geom_half_boxplot(
            position = position_nudge(x = 0.35),
            fill = fill,
            color = color,
            alpha = alpha,
            side = "r",
            outlier.shape = NA,
            center = TRUE,
            errorbar.draw = FALSE,
            width = 0.15,
            show.legend = FALSE
        ) +
      scale_color_manual(values =  color) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      coord_flip() +
      ylim(c(-20, 20)) +
      xlim(c(0.8, 1.44)) +
      theme_void() +
      theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm")) 
    
    return(figure_1x1_vertical)
}

# my_raincloud(select(iris, Sepal.Width))
