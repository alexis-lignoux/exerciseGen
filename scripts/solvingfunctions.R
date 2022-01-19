solveExercise <- function(coefficients = database$coefficients){
    
    polyDegre <- ifelse(coefficients[[1]] == 0, length(coefficients) - 2, length(coefficients) - 1)
    
    if (polyDegre == 3){
        derivee <- list(
            "a" = coefficients$a * 3,
            "b" = coefficients$b * 2,
            "c" = coefficients$c * 1
        )
        
        delta <- derivee$b**2-4*derivee$a*derivee$c
        
        if (delta > 0){
            
            extremum <- list(
                "x1" = (-derivee$b-sqrt(delta))/(2*derivee$a),
                "x2" = (-derivee$b+sqrt(delta))/(2*derivee$a)
            )
            
        } else if (delta == 0){
            extremum <- -derivee$b/(2*derivee$a)
        } else if (delta < 0){
            extremum <- NULL
        }
        
        exEquation <- paste0(
            "f(x)=", ifelse(coefficients$a < 0, "-", ""), ifelse(abs(coefficients$a) != 1, abs(coefficients$a), ""), "x^3",
            ifelse(coefficients$b != 0, paste0(ifelse(sign(coefficients$b) == -1, "-", "+"), ifelse(abs(coefficients$b) != 1, abs(coefficients$b), ""), "x^2"), ""),
            ifelse(coefficients$c != 0, paste0(ifelse(sign(coefficients$c) == -1, "-", "+"), ifelse(abs(coefficients$c) != 1, abs(coefficients$c), ""), "x"), ""),
            ifelse(coefficients$d != 0, paste0(ifelse(sign(coefficients$d) == -1, "-", "+"), abs(coefficients$d)), "")
        )
        
        formuleDerivee <- paste0(
            "f'(x)=", ifelse(derivee$a < 0, "-", ""), ifelse(abs(derivee$a) != 1, abs(derivee$a), ""), "x^2",
            ifelse(derivee$b != 0, paste0(ifelse(sign(derivee$b) == -1, "-", "+"), ifelse(abs(derivee$b) != 1, abs(derivee$b), ""), "x"), ""),
            ifelse(derivee$c != 0, paste0(ifelse(sign(derivee$c) == -1, "-", "+"), abs(derivee$c)), "")
        )
    } else {
        
        derivee <- list(
            "a" = coefficients$a * 2,
            "b" = coefficients$b * 1
        )
        
        extremum <- -derivee$b/derivee$a
        
        exEquation <- paste0(
            "f(x)=", ifelse(coefficients$a < 0, "-", ""), ifelse(abs(coefficients$a) != 1, abs(coefficients$a), ""), "x^2",
            ifelse(coefficients$b != 0, paste0(ifelse(sign(coefficients$b) == -1, "-", "+"), ifelse(abs(coefficients$b) != 1, abs(coefficients$b), ""), "x"), ""),
            ifelse(coefficients$c != 0, paste0(ifelse(sign(coefficients$c) == -1, "-", "+"), abs(coefficients$c)), "")
        )
        
        formuleDerivee <- paste0(
            "f'(x)=", ifelse(derivee$a < 0, "-", ""), ifelse(abs(derivee$a) != 1, abs(derivee$a), ""), "x",
            ifelse(derivee$b != 0, paste0(ifelse(sign(derivee$b) == -1, "-", "+"), abs(derivee$b)), "")
        )
    }
    
    fullString <- paste0(
        "\\textit{Déterminer le tableau de variation de la fonction suivante :}\n\n",
        "\\begin{center}\n",
        "\\large\n",
        paste0("$", exEquation, "$\n"),
        "\\end{center}\n",
        "\\vspace{1.2cm}\n",
        "\\normalsize\n",
        "On commence par déterminer la dérivée de notre fonction. Dans notre cas la dérivée s'écrit :\n\n",
        "\\begin{center}\n", "
        \\large\n",
        paste0("$", formuleDerivee, "$\n"),
        "\\end{center}\n",
        "\\bigskip\n",
        "\\normalsize\n",
        "L'étape suivante consiste à déterminer quand la dérivée s'annule, c'est à dire résoudre l'équation :\n\n",
        "\\begin{center}\n",
        "\\large\n",
        "$f'(x)=0$\n\n",
        "$\\Leftrightarrow$  $", 
        paste0(gsub("f'(x)=", "", formuleDerivee, fixed = TRUE), "=0"), "$\n\n",
        "\\end{center}\n",
        "\\smallskip\n",
        "\\normalsize\n",
        paste0("Dans notre cas, $f'(x)$ est un polynôme de degré $", polyDegre - 1, "$ donc ")
    )
    
    if (polyDegre == 3){
        fullString <- paste0(
            fullString,
            "on calcule le $\\Delta$ :\n\n",
            "\\large\n\n",
            "\\phantom{$\\Rightarrow$} $\\Delta=b^2-4ac$\n\n",
            paste0("$\\Leftrightarrow$ $\\Delta=(", derivee$b, ")^2-4\\times(", derivee$a, ")\\times(", derivee$c, ")$\n\n"),
            paste0("$\\Leftrightarrow$ $\\Delta=", delta, "$\n\n")
        )
        
        if (delta > 0){
            fullString <- paste0(
                fullString,
                "$\\Leftrightarrow$ $\\Delta>0$\n\n",
                "\\smallskip\n\n",
                "\\normalsize\n\n",
                "On a donc deux solutions à l'équation $f'(x)=0$. Déterminons à présent ces deux solutions :\n\n",
                "\\large\n\n",
                "\\phantom{$\\Rightarrow$} $x_1=\\frac{-b-\\sqrt{\\Delta}}{2a}$\n\n",
                paste0("$\\Leftrightarrow$ $x_1=\\frac{-(", derivee$b, ")-\\sqrt{", delta, "}}{2\\times(", derivee$a,")}$\n\n"),
                paste0("$\\Leftrightarrow$ $x_1=", extremum$x1, "$\n\n")
            )
            if (round(extremum$x1, digits = 4) != round(extremum$x1, digits = 8)){
                fullString <- paste0(
                    fullString,
                    paste0("$\\Leftrightarrow$ $x_1\\approx ", round(extremum$x1, digits = 3), "$ (à $10^{-3}$ près).\n\n")
                )
            }
            fullString <- paste0(
                fullString,
                "\\medskip\n\n",
                "\\phantom{$\\Rightarrow$} $x_2=\\frac{-b+\\sqrt{\\Delta}}{2a}$\n\n",
                paste0("$\\Leftrightarrow$ $x_2=\\frac{-(", derivee$b, ")+\\sqrt{", delta, "}}{2\\times(", derivee$a,")}$\n\n"),
                paste0("$\\Leftrightarrow$ $x_2=", extremum$x2, "$\n\n")
            )
            if (round(extremum$x2, digits = 4) != round(extremum$x2, digits = 8)){
                fullString <- paste0(
                    fullString,
                    paste0("$\\Leftrightarrow$ $x_2\\approx ", round(extremum$x2, digits = 3), "$ (à $10^{-3}$ près).\n\n")
                )
            }
        } else if (delta == 0){
            fullString <- paste0(
                fullString,
                "\\normalsize\n\n",
                "On a donc une seule solution à l'équation $f'(x)=0$. Déterminons à présent cette unique solution :\n\n",
                "\\large\n\n",
                "\\phantom{$\\Rightarrow$} $x_0=\\frac{-b}{2a}$\n\n",
                paste0("$\\Leftrightarrow$ $x_0=\\frac{-(", derivee$b, ")}{2\\times(", derivee$a,")}$\n\n"),
                paste0("$\\Leftrightarrow$ $x_0=", extremum, "$\n\n")
            )
            if (round(extremum, digits = 4) != round(extremum, digits = 8)){
                fullString <- paste0(
                    fullString,
                    paste0("$\\Leftrightarrow$ $x_0\\approx ", round(extremum, digits = 3), "$ (à $10^{-3}$ près).\n\n")
                )
            }
        } else {
            fullString <- paste0(
                fullString,
                "$\\Leftrightarrow$ $\\Delta<0$\n\n",
                "\\normalsize\n\n",
                "L'équation n'admet donc pas de solution dans $\\mathbf{R}$.\n\n"
            )
        }
    } else {
        fullString <- paste0(
            fullString,
            "on peut directement résoudre l'équation :\n\n",
            "\\large\n\n"
        )
        if (derivee$b != 0){
            fullString <- paste0(
                fullString,
                paste0("$\\Leftrightarrow$ $", ifelse(derivee$a < 0, "-", ""), ifelse(abs(derivee$a) != 1, abs(derivee$a), ""), "x=0-(", derivee$b, ")$\n\n")
            )
            if (derivee$a != 1){
                fullString <- paste0(
                    fullString,
                    paste0("$\\Leftrightarrow$ $", "x=\\frac{", paste0(ifelse(sign(derivee$b) == -1, "", "-"), abs(derivee$b)),"}{", derivee$a, "}$\n\n")
                )
            }
        } else {
            if (derivee$a != 1){
                fullString <- paste0(
                    fullString,
                    paste0("$\\Leftrightarrow$ $", "x=\\frac{0}{", derivee$a, "}$\n\n")
                )
            }
        }
        fullString <- paste0(
            fullString,
            paste0("$\\Leftrightarrow$ $x=", extremum, "$\n\n")
        )
        if (round(extremum, digits = 4) != round(extremum, digits = 8)){
            fullString <- paste0(
                fullString,
                paste0("$\\Leftrightarrow$ $x\\approx ", round(extremum, digits = 3), "$ (à $10^{-3}$ près).\n\n")
            )
        }
    }
    
    if (length(extremum) > 0){
        fullString <- paste0(
            fullString,
            "\\normalsize\n\n",
            "\\bigskip\n\n",
            ifelse(
                length(extremum) == 2,
                "On calcule les valeurs prises par la fonction en $x_1$ et $x_2$ :\n\n",
                ifelse(
                    polyDegre == 3,
                    "On calcule la valeur prise par la fonction en $x_0$ :\n\n",
                    "On calcule la valeur prise par la fonction en $x$ :\n\n"
                )
            ),
            "\\large\n\n"
        )
        
        if (length(extremum) == 2){
            fullString <- paste0(
                fullString,
                "\\phantom{$\\Leftrightarrow$} $",
                gsub("x", "x_1", exEquation, fixed = TRUE),
                "$\n\n",
                "$\\Leftrightarrow$ $",
                gsub("x", paste0("(", round(extremum$x1, digits = 3), ")"), gsub("f(x)=", "fx=", exEquation, fixed = TRUE), fixed = TRUE),
                "$\n\n",
                "$\\Leftrightarrow$ $",
                gsub("x", round(extremum$x1, digits = 3), "f(x)=", fixed = TRUE)
            )
            
            formulaToEvaluate <- gsub("+*", "+", gsub("-*", "-", gsub("^", "**", gsub("x", paste0("*(", extremum$x1, ")"), gsub("f(x)=", "", exEquation, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
            if(substr(formulaToEvaluate, 1, 1) == "*"){
                formulaToEvaluate <- substr(formulaToEvaluate, 2, nchar(formulaToEvaluate))
            }
            fx1 <- eval(parse(text = formulaToEvaluate))
            
            fullString <- paste0(
                fullString,
                fx1,
                "$\n\n"
            )
            
            if (round(fx1, digits = 4) != round(fx1, digits = 8)){
                fullString <- paste0(
                    fullString,
                    paste0(
                        "$\\Leftrightarrow$ $",
                        gsub("x", round(extremum$x1, digits = 3), "f(x)", fixed = TRUE),
                        "\\approx ", round(fx1, digits = 3), "$ (à $10^{-3}$ près).\n\n"
                    )
                )
            }
            
            fullString <- paste0(
                fullString,
                "\\medskip\n\n",
                "\\phantom{$\\Leftrightarrow$} $",
                gsub("x", "x_2", exEquation, fixed = TRUE),
                "$\n\n",
                "$\\Leftrightarrow$ $",
                gsub("x", paste0("(", round(extremum$x2, digits = 3), ")"), gsub("f(x)=", "fx=", exEquation, fixed = TRUE), fixed = TRUE),
                "$\n\n",
                "$\\Leftrightarrow$ $",
                gsub("x", round(extremum$x2, digits = 3), "f(x)=", fixed = TRUE)
            )
            
            formulaToEvaluate <- gsub("+*", "+", gsub("-*", "-", gsub("^", "**", gsub("x", paste0("*(", extremum$x2, ")"), gsub("f(x)=", "", exEquation, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
            if(substr(formulaToEvaluate, 1, 1) == "*"){
                formulaToEvaluate <- substr(formulaToEvaluate, 2, nchar(formulaToEvaluate))
            }
            fx2 <- eval(parse(text = formulaToEvaluate))
            
            fullString <- paste0(
                fullString,
                fx2,
                "$\n\n"
            )
            
            if (round(fx2, digits = 4) != round(fx2, digits = 8)){
                fullString <- paste0(
                    fullString,
                    paste0(
                        "$\\Leftrightarrow$ $",
                        gsub("x", round(extremum$x2, digits = 3), "f(x)", fixed = TRUE),
                        "\\approx ", round(fx2, digits = 3), "$ (à $10^{-3}$ près).\n\n"
                    )
                )
            }
        } else {
            fullString <- paste0(
                fullString,
                "\\phantom{$\\Leftrightarrow$} $",
                gsub("x", ifelse(polyDegre == 3, "x_0", "x"), exEquation, fixed = TRUE),
                "$\n\n",
                "$\\Leftrightarrow$ $",
                gsub("x", paste0("(", round(extremum, digits = 3), ")"), gsub("f(x)=", "fx=", exEquation, fixed = TRUE), fixed = TRUE),
                "$\n\n",
                "$\\Leftrightarrow$ $",
                gsub("x", round(extremum, digits = 3), "f(x)=", fixed = TRUE)
            )
            
            formulaToEvaluate <- gsub("+*", "+", gsub("-*", "-", gsub("^", "**", gsub("x", paste0("*(", extremum, ")"), gsub("f(x)=", "", exEquation, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
            if(substr(formulaToEvaluate, 1, 1) == "*"){
                formulaToEvaluate <- substr(formulaToEvaluate, 2, nchar(formulaToEvaluate))
            }
            
            fx <- eval(parse(text = formulaToEvaluate))
            
            fullString <- paste0(
                fullString,
                fx,
                "$\n\n"
            )
            
            if (round(fx, digits = 4) != round(fx, digits = 8)){
                fullString <- paste0(
                    fullString,
                    paste0(
                        "$\\Leftrightarrow$ $",
                        gsub("x", round(extremum, digits = 3), "f(x)", fixed = TRUE),
                        "\\approx ", round(fx, digits = 3), "$ (à $10^{-3}$ près).\n\n"
                    )
                )
            }
        }
    }
    
    fullString <- paste0(
        fullString,
        "\\normalsize\n\n",
        "\\vspace{1.2cm}\n\n",
        "On obtient le tableau de variation suivant :\n\n",
        "\\medskip\n\n",
        "\\large\n\n",
        "\\begin{center}\n\n",
        "\\begin{tikzpicture}\n\n",
        "\\tkzTabInit{$x$ / 1.5 , $f'(x)$ / 1.5, $f(x)$ / 2.5}{$-\\infty$,"
    )
    
    if (length(extremum) > 0){
        if (length(extremum) > 1){
            fullString <- paste0(
                fullString,
                paste0(
                    "$",
                    round(min(c(extremum$x1, extremum$x2)), digits = 3),
                    "$, $",
                    round(max(c(extremum$x1, extremum$x2)), digits = 3),
                    "$, "
                )
            )
        } else {
            fullString <- paste0(
                fullString,
                paste0("$", round(extremum, digits = 3), "$, ")
            )
        }
    }
    
    fullString <- paste0(
        fullString,
        "$+\\infty$}\n",
        "\\tkzTabLine{, "
    )
    
    if (length(extremum) >= 1){
        if (length(extremum) > 1){
            if (derivee$a > 0){
                signs <- c("+", "-", "+")
                arrows <- c("$-\\infty$",
                            ifelse(extremum$x1 > extremum$x2, paste0("$", round(fx2, digits = 3), "$"), paste0("$", round(fx1, digits = 3), "$")),
                            ifelse(extremum$x1 > extremum$x2, paste0("$", round(fx1, digits = 3), "$"), paste0("$", round(fx2, digits = 3), "$")),
                            "$+\\infty$")
            } else {
                signs <- c("-", "+", "-")
                arrows <- c("$+\\infty$",
                            ifelse(extremum$x1 > extremum$x2, paste0("$", round(fx2, digits = 3), "$"), paste0("$", round(fx1, digits = 3), "$")),
                            ifelse(extremum$x1 > extremum$x2, paste0("$", round(fx1, digits = 3), "$"), paste0("$", round(fx2, digits = 3), "$")),
                            "$-\\infty$")
            }
        } else {
            if (polyDegre == 3){
                if (derivee$a > 0){
                    signs <- c("+", "+")
                    arrows <- c("$-\\infty$",
                                # paste0("$", round(fx, digits = 3), "$"),
                                "",
                                "$+\\infty$")
                } else {
                    signs <- c("-", "-")
                    arrows <- c("$+\\infty$",
                                # paste0("$", round(fx, digits = 3), "$"),
                                "",
                                "$-\\infty$")
                }
            } else {
                if (derivee$a > 0){
                    signs <- c("-", "+")
                    arrows <- c("$+\\infty$",
                                paste0("$", round(fx, digits = 3), "$"),
                                "$+\\infty$")
                } else {
                    signs <- c("+", "-")
                    arrows <- c("$-\\infty$",
                                paste0("$", round(fx, digits = 3), "$"),
                                "$-\\infty$")
                }
            }
        }
    } else {
        if (polyDegre == 3){
            if (derivee$a > 0){
                signs <- c("+")
                arrows <- c("$-\\infty$",
                            "$+\\infty$")
            } else {
                signs <- c("-")
                arrows <- c("$+\\infty$",
                            "$-\\infty$")
            }
        } else {
            
        }
    }
    
    if (signs[1] == "+"){
        signs2 <- c("-", signs)
    } else {
        signs2 <- c("+", signs)
    }
    
    if (length(extremum) == 1 && polyDegre == 3){
        signs2[2] <- "R"
    }
    
    fullString <- paste0(
        fullString,
        paste(signs, collapse = ", z, "),
        ", }\n",
        "\\tkzTabVar{",
        paste(paste(signs2,arrows, sep = "/ "), collapse = ", "),
        "}\n"
    )
    
    if (length(extremum) == 1 && polyDegre == 3){
        fullString <- paste0(
            fullString,
            "\\tkzTabIma{1}{3}{2}{",
            paste0("$", round(fx, digits = 3), "$"),
            "}\n"
        )
    }
    
    fullString <- paste0(
        fullString,
        "\\end{tikzpicture}\n\n",
        "\\medskip\n",
        "\\normalsize\n",
        "Tableau de variation de la fonction $f(x)$ :\n\n",
        "\\end{center}\n\n",
        "\\vspace{1.5cm}\n\n"
    )
    
    if (length(extremum) == 2){
        spread     <- abs(extremum$x1 - extremum$x2)
        minBounder <- (min(extremum$x1, extremum$x2) - spread/1.5)
        maxBounder <- (max(extremum$x1, extremum$x2) + spread/1.5)
    } else if (length(extremum) == 1){
        minBounder <- (extremum - 2)
        maxBounder <- (extremum + 2)
    } else {
        if (polyDegre == 3){
            derivee2 <- list("a" = derivee$a*2,
                             "b" = derivee$b)
            extrem <- -derivee2$b/derivee2$a
            minBounder <- (extrem - 2)
            maxBounder <- (extrem + 2)
        } else {
            minBounder <- -4
            maxBounder <- 4
        }
    }
    
    fullString <- paste0(
        fullString,
        "\\newpage\n\n",
        "\\begin{center}\n",
        "\\begin{tikzpicture}\n",
        "\\begin{axis}[\n",
        "axis lines = left,\n",
        "xlabel = \\(x\\),\n",
        "ylabel = {\\(f(x)\\)},\n",
        "]\n",
        "%Below the blue function is defined\n",
        "\\addplot [\n",
        "domain=",
        minBounder,
        ":",
        maxBounder,
        ",\n",
        "samples=100,\n",
        "color=blue,\n",
        "]\n",
        "{"
    )
    
    graphformula <- gsub("+*", "+", gsub("-*", "-", gsub("x", "*x", gsub("f(x)=", "", exEquation, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
    if(substr(graphformula, 1, 1) == "*"){
        graphformula <- substr(graphformula, 2, nchar(graphformula))
    }
    
    fullString <- paste0(
        fullString,
        graphformula,
        "};\n",
        "\\addlegendentry{\\(",
        gsub("f(x)=", "", exEquation, fixed = TRUE),
        "\\)}\n",
        "\\end{axis}\n",
        "\\end{tikzpicture}\n",
        "\\medskip\n",
        "\\normalsize\n",
        "Représentation graphique de la fonction $f(x)$ :\n\n",
        "\\end{center}"
    )
    
    return(fullString)
}


# cat(solveExercise(coefficients = list("a" = 4, "b" = 12, "c" = 12, "d" = -9)))
# cat(solveExercise(coefficients = list("a" = 0, "b" = 7, "c" = 1, "d" = -3)))

