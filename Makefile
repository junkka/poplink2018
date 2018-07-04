SD = scripts
DD = data
RD = data-raw
CD = .cache
CMD = R CMD BATCH --no-save
RAWDATA1 = $(DD)/barn.rda $(DD)/narvaro.rda $(DD)/vigsel.rda 
RAWDATA2 = $(DD)/mor_person.rda $(DD)/partner_person.rda $(DD)/partner.rda 

all: $(DD)/eh_data.rda

clean:
	-rm -f *.Rout

cleanall: 
	-rm -f *.Rout
	-rm -f $(DD)/*
	-rm -f $(CD)/*

$(DD)/eh_data.rda: $(SD)/ehd_fill.R $(CD)/ehd_dens.rda
	$(CMD)  $(SD)/ehd_fill.R

$(CD)/ehd_dens.rda: $(SD)/ehd_dens.R $(CD)/ehd_mig.rda
	$(CMD) $(SD)/ehd_dens.R

$(CD)/ehd_mig.rda: $(SD)/ehd_mig.R $(CD)/ehd_exp.rda
	$(CMD) $(SD)/ehd_mig.R

$(CD)/ehd_exp.rda: $(SD)/ehd_exp.R $(DD)/ehd_loc.rda $(DD)/voroni_spdf.rda
	$(CMD) $(SD)/ehd_exp.R

$(DD)/voroni_spdf.rda: $(SD)/voro-partition.R $(DD)/ehd_loc.rda
	$(CMD) $(SD)/voro-partition.R

$(DD)/ehd_loc.rda: $(SD)/ehd_loc.R $(CD)/moves.rda $(CD)/ehd_va.rda 
	$(CMD) $(SD)/ehd_loc.R
	
$(CD)/moves.rda: $(SD)/moves.R $(DD)/flytt.rda
	$(CMD) $(SD)/moves.R
	
$(CD)/ehd_va.rda: $(SD)/ehd_va.R $(CD)/ehd_class.rda
	$(CMD) $(SD)/ehd_va.R

$(CD)/ehd_class.rda: $(SD)/ehd_class.R $(CD)/ehd.rda
	$(CMD) $(SD)/ehd_class.R

$(CD)/ehd.rda: $(SD)/ehd_base.R $(RAWDATA) $(DD)/children.rda $(DD)/migration.rda $(DD)/person.rda
	$(CMD) $(SD)/ehd_base.R

$(DD)/children.rda: $(SD)/children.R $(DD)/barn.rda $(RAWDATA2)
	$(CMD) $(SD)/children.R

$(DD)/migration.rda: $(SD)/migration.R $(DD)/narvaro.rda $(RAWDATA2)
	$(CMD) $(SD)/migration.R

$(DD)/person.rda: $(SD)/person.R $(DD)/mor_person.rda $(DD)/partner_person.rda $(RAWDATA2)
	$(CMD) $(SD)/person.R

$(RAWDATA2): $(RD)/import_u16003.R $(RAWDATA1)
	$(CMD) $(RD)/import_u16003.R

$(RAWDATA1): $(RD)/import_u16006.R
	$(CMD) $(RD)/import_u16006.R
