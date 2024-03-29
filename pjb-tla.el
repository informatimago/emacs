
(defvar *acronyms*
  '(

    ("AEAD" "authenticated encryption with associated data")
    ("AES" "advanced encryption standard")
    ("AIFS" "acknowledgment interframe spacing")
    ("ANPI" "average noise power indicator")
    ("AR" "acknowledgment request")
    ("ASK" "amplitude shift keying")
    ("ASN" "absolute slot number")
    ("ATI" "allowed transmission interval")
    ("ACK" "acknowledgment")
    ("BCH" "Bose Chaudhuri Hocquenghem")
    ("BDE" "bit differential encoding")
    ("BLE" "battery life extension")
    ("BOP" "beacon only period")
    ("BPM" "burst position modulationStandard for Low-Rate Wireless Networks")
    ("BPSK" "binary phase-shift keying")
    ("BSN" "beacon sequence number")
    ("CAP" "contention access period")
    ("CBC-MAC" "cipher block chaining message authentication code")
    ("CCA" "clear channel assessment")
    ("CCM" "counter mode encryption and cipher block chaining message authentication code")
    ("CCM*" "extension of counter mode encryption and cipher block chaining message authentication code")
    ("CFP" "contention-free period CID company identifier CMB China medical band CP cyclic prefix")
    ("CSK" "chirp-shift keying")
    ("CSL" "coordinated sampled listing")
    ("CSM" "common signaling mode")
    ("CSMA-CA" "carrier sense multiple access with collision avoidance")
    ("CSS" "chirp spread spectrum DA device announcement DAA detect and avoid")
    ("CoU" "chirp on ultra-wide band CRC cyclic redundancy check CS continuous spectrum")
    ("DBS" "dedicated beacon slot DEMUX de-multiplexer")
    ("DFT" "discrete Fourier transform")
    ("DPS" "dynamic preamble selection")
    ("DPSK" "differential phase-shift keying")
    ("DQCSK" "differential quadrature chirp-shift keying")
    ("DQPSK" "differential quadrature phase-shift keying")
    ("DSME" "deterministic and synchronous multichannel extension DSN data sequence number")
    ("DSSS" "direct sequence spread spectrum")
    ("EBSN" "enhanced beacon sequence number")
    ("ED" "energy detection (also in 6.2.8, extended duration) EIRP effective isotropic radiated power")
    ("ESDU" "encapsulated service data unit")
    ("EUI-64" "64-bit extended unique identifier")
    ("EVM" "error-vector magnitude")
    ("Enh-Ack" "enhanced acknowledgment")
    ("FCS" "frame check sequence")
    ("FEC" "forward error correction")
    ("FFD" "full-function device")
    ("FICS" "fragment integrity check sequence")
    ("FSCD" "fragment sequence context description")
    ("FSK" "frequency shift keying")
    ("FoM" "figure of merit")
    ("Frak" "fragment acknowledgment")
    ("GDB" "geolocation database")
    ("GFSK" "Gaussian frequency-shift keying")
    ("GMSK" "Gaussian-filtered minimum shift keying")
    ("GTS" "guaranteed timeslot")
    ("HCS" "header check sequence")
    ("HRP" "high rate pulse repetition frequency")
    ("I-RIT" "implicit receiver initiated transmission")
    ("ID" "identifier")
    ("IDFT" "inverse discrete Fourier transform")
    ("IE" "information element")
    ("IEEE" "Standard for Low-Rate Wireless Networks")
    ("IEEE" "Standard for Low-Rate Wireless Networks")
    ("IEEE" "Standard for Low-Rate Wireless Networks")
    ("IEEE" "Std 802.15.4-2020")
    ("IEEE" "Std 802.15.4-2020")
    ("IFS" "interframe space or spacing")
    ("IPI" "idle power indicator")
    ("ISR" "interference-to-signal ratio")
    ("Imm-Ack" "immediate acknowledgment")
    ("LBT" "listen before talk")
    ("LCP" "linear combination of pulses")
    ("LE" "low energy")
    ("LECIM" "low-energy, critical infrastructure monitoring")
    ("LEIP" "location enhancing information postamble LFSR linear feedback shift register")
    ("LIFS" "long interframe spacing")
    ("LMR" "land mobile radio")
    ("LQI" "link quality indication")
    ("LR-WPAN" "low-rate wireless personal area network LSB least significant bit")
    ("LRP" "low rate pulse repetition frequency")
    ("LTF" "long training field")
    ("MAC" "medium access control")
    ("MBAN" "medical body area network")
    ("MCPS" "MAC common part sublayer")
    ("MCPS-SAP" "MAC common part sublayer service access point MCS modulation and coding scheme")
    ("MD" "multi-superframe duration")
    ("MDSSS" "multiplexed direct sequence spread spectrum")
    ("MFR" "MAC footer")
    ("MHR" "MAC header")
    ("MIC" "message integrity code")
    ("MLME" "MAC sublayer management entity")
    ("MLME-SAP" "MAC sublayer management entity service access point MPM multi-PHY management")
    ("MPDU" "MAC protocol data unit")
    ("MSB" "most significant bit")
    ("MSDU" "MAC service data unit")
    ("MSK" "minimum shift keying")
    ("NRNSC" "nonrecursive and nonsystematic code")
    ("O-QPSK" "offset quadrature phase-shift keying")
    ("OFDM" "orthogonal frequency division multiplexing")
    ("OOK" "on-off keying")
    ("OUI" "organizationally unique identifier")
    ("OVSF" "orthogonal variable spreading factor")
    ("P-FSK" "position-based frequency shift keying")
    ("PAN" "personal area network")
    ("PCA" "priority channel access")
    ("PD-SAP" "physical layer data service access point")
    ("PER" "packet error rate")
    ("PHR" "PHY header")
    ("PHY" "physical layer")
    ("PIB" "personal area network information base")
    ("PICS" "protocol implementation conformance statement PLME physical layer management entity")
    ("PLME-SAP" "physical layer management entity service access point PN pseudo-random noise")
    ("PPDU" "PHY protocol data unit")
    ("PPM" "pulse position modulation")
    ("PRBS" "pseudo-random binary sequence PRF pulse repetition frequency")
    ("PSD" "power spectral density")
    ("PSDU" "PHY service data unit")
    ("QAM" "quadrature amplitude modulation")
    ("QPSK" "quadrature phase-shift keying")
    ("RCC" "rail communications and control")
    ("RCCN" "rail communications and control network RCPI received channel power indicator")
    ("RDEV" "ranging-capable device")
    ("RF" "radio frequency")
    ("RFD" "reduced-function device")
    ("RFD-RX" "reduced function device—receive only RFD-TX reduced function device—transmit only RFID radio frequency identification RFRAME ranging frame")
    ("RIT" "receiver initiated transmission")
    ("RMARKER" "ranging marker")
    ("RS-GFSK" "rate switch Gaussian frequency shift keying RSNI received signal noise indicator")
    ("RSC" "recursive and systematic code")
    ("RSSI" "receive signal strength indicator RX receive or receiver")
    ("SAB" "slot allocation bitmap")
    ("SD" "superframe duration")
    ("SF" "spreading factor")
    ("SFD" "start-of-frame delimiter")
    ("SHR" "synchronization header")
    ("SIFS" "short interframe spacing")
    ("SNR" "signal-to-noise ratio")
    ("SPC" "super PAN coordinator")
    ("SRM" "spectrum resource measurement")
    ("STF" "short training field")
    ("SUN" "smart utility network")
    ("SiPC" "single parity check")
    ("TASK" "ternary amplitude shift keying")
    ("TBPC" "turbo product code")
    ("TID" "transaction identifier")
    ("TMCTP" "TVWS multichannel cluster tree PAN TPC transmission power control")
    ("TRLE" "timeslot relaying based link extension TSCH timeslotted channel hopping")
    ("TVWS" "television white space")
    ("TX" "transmit or transmitter")
    ("UWB" "ultra-wide band")
    ("WPAN" "wireless personal area network")
    ("TLV" "Type Length Value Encoding")
    ))

(defun explain-tla-search (tla-string)
  (interactive "sTla Search: ")
  (browse-url (format *tla-search-url* tla-string)))

(defun tla-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'tla-search))

"" ""

Wireless Personal Area Network
wireless personal area network

wpan
WPAN

