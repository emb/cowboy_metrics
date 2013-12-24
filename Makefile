PROJECT = cowboy_metrics

default: mibs all

include erlang.mk

## Extensions to compile the mib files

MIB_FILES = COWBOY-MIB.mib

MIB_DIR = mibs/
MIB_OUTPUT_DIR = priv/mibs
HRL_OUTPUT_DIR = include

ERLC_MIB_OPTS = +'{il, ["otp_mibs/priv/mibs/"]}' +'{verbosity, info}'

TARGET_FILES = \
  $(MIB_FILES:%.mib=$(MIB_OUTPUT_DIR)/%.bin) \
  $(MIB_FILES:%.mib=$(HRL_OUTPUT_DIR)/%.hrl)


clean-mibs:
	$(gen_verbose) rm -f $(TARGET_FILES)


mibs: $(TARGET_FILES)


$(MIB_OUTPUT_DIR)/%.bin:
	@mkdir -p $(MIB_OUTPUT_DIR)
	$(erlc_verbose) erlc -v $(ERLC_MIB_OPTS) -o $(MIB_OUTPUT_DIR) \
	$(addprefix $(MIB_DIR), $(MIB_FILES))

$(HRL_OUTPUT_DIR)/%.hrl: $(MIB_OUTPUT_DIR)/%.bin
	@mkdir -p $(HRL_OUTPUT_DIR)
	$(erlc_verbose) erlc -v $(ERLC_MIB_OPTS) -o $(HRL_OUTPUT_DIR) $<
