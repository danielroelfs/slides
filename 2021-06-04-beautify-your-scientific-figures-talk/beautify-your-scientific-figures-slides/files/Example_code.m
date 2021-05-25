function PlotPalmerPenguins() 

penguins = readtable("/Users/dtroelfs/Dropbox/NORMENT/scripts/slides/2021-06-04-beautify_scientific_figures_talk/files/penguins.csv");

species = categorical(penguins.species);
color_idx = grp2idx(species);

scatter(penguins.bill_length_mm, penguins.bill_depth_mm, [], color_idx, 'filled')


end