import os
import sys
 
#List of default configurations of actions
config = ['Standard',              # Simple beam propagation (to NI and WI), with auxiliary beams
          'ResonantCavities',      # With cavities at resonance
          'B1_B5_B5p',             # B5 and B5p to SDBs and SIBs
          'B1_B9_B9p',             # Secondary beams from NICP to SDBs, SIBs and SPRB
          'B1_B9_B9p_B6_B6p',      # Secondary beams from NICP and WICP to SDBs, SIBs and SPRB          
          'Mirko','Romain','Loic'  # Some user's working configurations
          ]
 
 
printDefault = 'rs act rd_5 an2_5  w2t_5 w2s_5  w0t_4 w0s_4  z2t_3 z2s_3  Gp2_3 ipp_4 lb_24'
printMirko   = 'rs act rd_5 an2_5  w2t_5 w2s_5  w0t_4 w0s_4  z2t_3 z2s_3  z0t_3 z0s_3 Gp2t_3 Gp2s_3 ipp_4 lb_24'
printRomain  = 'rs act rd_5 an2_5  dx_4 dy_4 x2_6 y2_6 w2t_4 w2s_4  w0t_4 w0s_4  z2t_4 z2s_4 z0t_4 z0s_4 Gp2_3 ipp_4 lb_24'
printLoic    = 'rs act rd_5 an2_5  dx_6 dy_6   x2_6  y2_6   ipp_4 lb_24'
 
 
# ----------------------------------------------
# Select the configuration to be simulated
 
#configuration  = 'Standard'
#configuration  = 'ResonantCavities'
#configuration  = 'B1_B5_B5p'
#configuration  = 'B1_B9_B9p'
#configuration  = 'B1_B9_B9p_B6_B6p'
 
 
#Pick print configuration (none = default)
#configuration  = 'Mirko'
configuration  = 'Romain'
#configuration  = 'Loic'
 
# Select print
if configuration == 'Mirko':
        printOptions = printMirko
elif configuration == 'Romain':
        printOptions = printRomain
elif configuration == 'Loic':
        printOptions = printLoic
else:
        printOptions = printRomain
 
 
 
# Define here the command to be launched when the ocd file is ready
 
#command = '$HOME/bin/occr  AdV_Layout.f95'
command = '/home1/bonnand/bin/occr  AdV_Layout.f95  > tmp.txt ; ps2pdf AdV_Layout_ITF.ps ; ps2pdf AdV_Layout_InputBenches.ps ; ps2pdf AdV_Layout_EndBenches.ps ; ps2pdf AdV_Layout_SDBs.ps; ps2pdf AdV_Layout_SQBs.ps;'
#command = 'occr  AdV_Layout.f95  > tmp.txt'
 
# ----------------------------------------------
 
if configuration not in config:
    sys.exit('ERROR: Configuration  '  + configuration + '  not defined --> check !!')
 
 
#Prepare the actions to be applied to the different mirrors
#"Standard" configuration: main beam propagation to all benches
# include the propagation of the auxiliary beams
 
print " "
if configuration is 'Standard':
    print "Standard configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'srtt',
            'actionPRMback': 't',
            'actionPOPfront':'tsrt',
            'actionPOPback': 't',
           
            'actionBSfront':'{srt}3r',
            'actionBSback': 't',
           
            'actionWIfront':  't',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  'str',
            'actionWEback':   't{str}t',
           
            'actionNIfront':  '{t}2r',
            'actionNIback':   't',
            'actionNICPfront':'t',
            'actionNICPback': 't',
           
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',
           
            'actionSRfront':  't',
            'actionSRback':   't',
           
            'actionSDB1Md':   '{r}2t'        #Dichroic mirror on SDB1, to separate Hartmann beam from YAG beam
            }
elif configuration is 'ResonantCavities':
   
    print "ResonantCavities configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'crtstrt',
            'actionPRMback': 't',
            'actionPOPfront':'t',
            'actionPOPback': 't',
           
            'actionBSfront':'stri(rnt){srt}',
            'actionBSback': 't',
           
            'actionWIfront':  'crtstr',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  'str',
            'actionWEback':   't',
           
            'actionNIfront':  'crtstr',
            'actionNIback':   't',
            'actionNICPfront':'t',
            'actionNICPback': 't',
 
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',        
           
            'actionSRfront':  't',
            'actionSRback':   't',
 
            'actionSDB1Md':   'r'
            }
   
elif configuration is 'B1_B5_B5p':
    print "'B1_B5_B5p_to_DET configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'{str}1t',
            'actionPRMback': 't',
            'actionPOPfront':'tsrt',
            'actionPOPback': 't',
           
            'actionBSfront':'t{str}2',
            'actionBSback': 't{srt}2t',
           
            'actionWIfront':  't',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  't',
            'actionWEback':   't',
           
            'actionNIfront':  't',
            'actionNIback':   't',
            'actionNICPfront':'t',
            'actionNICPback': 't',
           
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',
           
            'actionSRfront':  't',
            'actionSRback':   't',
           
            'actionSDB1Md':   'r'
            }
 
elif configuration is 'B1_B9_B9p':
    print "B1_B9_B9p configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'srtt',
            'actionPRMback': 't',
            'actionPOPfront':'t{str}3',
            'actionPOPback': 't',
           
            'actionBSfront':'t{str}3',
            'actionBSback': 't',
           
            'actionWIfront':  't',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  't',
            'actionWEback':   't',
           
            'actionNIfront':  'r',
            'actionNIback':   't',
            'actionNICPfront':'{srt}2t',
            'actionNICPback': 't',
           
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',
           
            'actionSRfront':  't',
            'actionSRback':   't',
 
            'actionSDB1Md':   'r'
            }
 
elif configuration is 'B1_B9_B9p_B6_B6p':
    print "B1_B9_B9p_B6_B6p configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'{srt}1t',
            'actionPRMback': 't',
            'actionPOPfront':'t{str}6',
            'actionPOPback': 't',
           
            'actionBSfront':'{str}7',
            'actionBSback': 't',
           
            'actionWIfront':  'r',
            'actionWIback':   't',
            'actionWICPfront':'{str}2t',
            'actionWICPback': 't',
           
            'actionWEfront':  't',
            'actionWEback':   't',
           
            'actionNIfront':  'r',
            'actionNIback':   't',
            'actionNICPfront':'{str}2t',
            'actionNICPback': 't',
           
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',            
           
            'actionSRfront':  't',
            'actionSRback':   't',
 
            'actionSDB1Md':   'r'
            }    
   
# ----------------------------------------------------------
# Mirko's configuration
elif configuration is 'Mirko':
    print "Mirko's configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'crtstrt',
            'actionPRMback': 't',
            'actionPOPfront':'t',
            'actionPOPback': 't',
           
            'actionBSfront':'stri(rnt){srt}',
            'actionBSback': 't',
           
            'actionWIfront':  'crtstr',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  'str',
            'actionWEback':   't',
           
            'actionNIfront':  'crtstr',
            'actionNIback':   't',
            'actionNICPfront':'t',
            'actionNICPback': 't',
 
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',
           
            'actionSRfront':  't',
            'actionSRback':   't',
           
            'actionSDB1Md':   'r'
            }
 
# ----------------------------------------------------------
# Romain's configuration
elif configuration is 'Romain':
    print "Romain's configuration"
    list = {'printOptions':printOptions,
 
            'actionPRMfront':'srtt',
            'actionPRMback': 't',
            'actionPOPfront':'t{srt}',
            'actionPOPback': 't',
           
            'actionBSback': 't{str}t',
            'actionBSfront':'{srt}r',
 
#           'actionBSfront':'{srt}r',
#           'actionBSback': 't',
           
            'actionWIfront':  't',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  'str',
            'actionWEback':   't{str}t',
           
            'actionNIfront':  '{t}2r',
            'actionNIback':   't',
            'actionNICPfront':'t',
            'actionNICPback': 't',
           
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',
           
            'actionSRfront':  't',
            'actionSRback':   't',
           
            'actionSDB1Md':   '{r}{srt}t'
         #   'actionSDB1Md':   '{str}3'
            }
   
# ----------------------------------------------------------
# Loic's configuration
elif configuration is 'Loic':
    print "Loic's configuration:"
    list = {'printOptions':printOptions,
           
            'actionPRMfront':'srtt',
            'actionPRMback': 't',
            'actionPOPfront':'tsrt',
            'actionPOPback': 't',
           
            'actionBSfront':'{srt}3r',
            'actionBSback': 't',
           
            'actionWIfront':  't',
            'actionWIback':   't',
            'actionWICPfront':'t',
            'actionWICPback': 't',
           
            'actionWEfront':  'str',
            'actionWEback':   't{str}t',
           
            'actionNIfront':  '{t}2r',
            'actionNIback':   't',
            'actionNICPfront':'t',
            'actionNICPback': 't',
           
            'actionNEfront':  '{str}2',
            'actionNEback':   't{str}t',
           
            'actionSRfront':  't',
            'actionSRback':   't',
           
            'actionSDB1Md':   '{r}2t'
            }
   
else:
    print "Unknown configuration !!"
    sys.exit('ERROR: Actions for configuration  '  + configuration + ' not defined !! Add it in the if/elif conditions')
#endif
 
print " "
print list
print " "
 
 
# --------------------------------------------------------------------
# Process the replacement of the action strings in the ocd file and start simulation
 
#Input ocd file name, with action strings to be replaced
finname = 'AdV_Layout.ocd'
#Output ocd file name, to be used for simulation
foutname = 'tmp_AdV_Layout.ocd'
 
#Open input file with the optical configuration
fin = open(finname, 'r')
print fin
print " "
 
#Open output file, to be written with the modified actions
fout = open(foutname,'w')
print fout
print " "
 
#Read the input file, replace the needed actions and write the output file for Optocad
nlines = 0
nreplacements = 0
for line in fin:
    line2 = line
    for old, new in list.iteritems():
        if line.find(old)>0:
            #print 'FOUND '+old+' in line '+line+'   -> Will replace '+old+' by '+new
            print '   -> Will replace '+old+' by '+new
            nreplacements = nreplacements+1
            line2 = line.replace(old,new)
            break
        else:
            line2 = line
   
    fout.write(line2)
    nlines = nlines+1
   
print 'Replaced %d in %d lines.' %( nreplacements , nlines)
   
fin.close()
fout.close()
 
 
#Start Optocad with this configuration.
print 'Running ' + command
os.system(command)