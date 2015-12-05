(ns my.stuff.fitlogger.sebas)

(def sebas-default (into (sorted-map)
                    {20151007 {:date 20151007,:weight 99.0,:bf 13.3,:water 63.5,:muscle 43.3}
                     20151008 {:date 20151008,:weight 99.5,:bf 13.3,:water 63.5,:muscle 43.2}
                     20151009 {:date 20151009,:weight 100.3,:bf 13.7,:water 63.2,:muscle 43.1}
                     20151010 {:date 20151010,:weight 99.3,:bf 13.2,:water 63.6,:muscle 43.2}
                     20151011 {:date 20151011,:weight 98.5,:bf 13.0,:water 63.7,:muscle 43.3}
                     20151012 {:date 20151012,:weight 100.8,:bf 13.8,:water 63.2,:muscle 43.1}
                     20151013 {:date 20151013,:weight 98.9,:bf 13.2,:water 63.6,:muscle 43.3}
                     20151014 {:date 20151014,:weight 98.0,:bf 13.0,:water 63.7,:muscle 43.4}
                     20151015 {:date 20151015,:weight 98.7,:bf 13.3,:water 63.5,:muscle 43.3}
                     20151016 {:date 20151016,:weight 98.5,:bf 13.1,:water 63.6,:muscle 43.3}
                     20151017 {:date 20151017,:weight 99.6,:bf 13.2,:water 63.6,:muscle 43.2}
                     20151018 {:date 20151018,:weight 99.4,:bf 13.5,:water 63.4,:muscle 43.2}
                     20151019 {:date 20151019,:weight 98.7,:bf 13.1,:water 63.6,:muscle 43.3}
                     20151020 {:date 20151020,:weight 99.4,:bf 13.5,:water 63.4,:muscle 43.2}
                     20151021 {:date 20151021,:weight 99.0,:bf 13.2,:water 63.6,:muscle 43.3}
                     20151022 {:date 20151022,:weight 99.7,:bf 13.5,:water 63.4,:muscle 43.2}
                     20151023 {:date 20151023,:weight 99.6,:bf 13.4,:water 63.4,:muscle 43.2}
                     20151024 {:date 20151024,:weight 99.3,:bf 13.3,:water 63.5,:muscle 43.2}
                     20151025 {:date 20151025,:weight 98.5,:bf 13.1,:water 63.6,:muscle 43.3}
                     20151026 {:date 20151026,:weight 99.4,:bf 13.2,:water 63.6,:muscle 43.2}
                     20151027 {:date 20151027,:weight 99.8,:bf 13.3,:water 63.5,:muscle 43.1}
                     20151028 {:date 20151028,:weight 99.2,:bf 13.4,:water 63.4,:muscle 43.2}
                     20151029 {:date 20151029,:weight 98.7,:bf 13.3,:water 63.5,:muscle 43.3}
                     20151030 {:date 20151030,:weight 98.3,:bf 13.1,:water 63.6,:muscle 43.4}
                     20151031 {:date 20151031,:weight 98.9,:bf 13.1,:water 63.6,:muscle 43.3}
                     20151101 {:date 20151101,:weight 98.7,:bf 13.2,:water 63.6,:muscle 43.3}
                     20151102 {:date 20151102,:weight 98.2,:bf 13.1,:water 63.6,:muscle 43.4}
                     20151103 {:date 20151103,:weight 98.9,:bf 13.1,:water 63.6,:muscle 43.3}
                     20151104 {:date 20151104,:weight 98.9,:bf 13.4,:water 63.4,:muscle 43.3}
                     20151105 {:date 20151105,:weight 98.7,:bf 13.3,:water 63.5,:muscle 43.3}
                     20151106 {:date 20151106,:weight 99.8,:bf 13.4,:water 63.4,:muscle 43.1}
                     20151110 {:date 20151110,:weight 98.4,:bf 13.1,:water 63.6,:muscle 43.3}
                     20151112 {:date 20151112,:weight 97.7,:bf 13.0,:water 63.8,:muscle 43.4}
                     20151113 {:date 20151113,:weight 98.9,:bf 13.3,:water 63.5,:muscle 43.3}
                     20151114 {:date 20151114,:weight 98.4,:bf 13.3,:water 63.5,:muscle 43.3}
                     20151115 {:date 20151115,:weight 97.6,:bf 13.1,:water 63.7,:muscle 43.5}
                     20151116 {:date 20151116,:weight 96.7,:bf 12.7,:water 64.0,:muscle 43.6}
                     20151117 {:date 20151117,:weight 96.1,:bf 12.4,:water 64.2,:muscle 43.7}
                     20151118 {:date 20151118,:weight 97.4,:bf 12.9,:water 63.9,:muscle 43.5}
                     20151119 {:date 20151119,:weight 97.3,:bf 13.0,:water 63.8,:muscle 43.5}
                     20151120 {:date 20151120,:weight 97.3,:bf 12.9,:water 63.9,:muscle 43.5}
                     20151121 {:date 20151121,:weight 99.2,:bf 13.2,:water 63.6,:muscle 43.2}
                     20151122 {:date 20151122,:weight 96.7,:bf 12.5,:water 64.1,:muscle 43.6}
                     20151123 {:date 20151123,:weight 97.8,:bf 13.0,:water 63.8,:muscle 43.4}
                     20151124 {:date 20151124,:weight 97.0,:bf 12.7,:water 64.0,:muscle 43.5}
                     20151125 {:date 20151125,:weight 96.6,:bf 12.7,:water 64.0,:muscle 43.6}
                     20151126 {:date 20151126,:weight 97.2,:bf 12.7,:water 64.0,:muscle 43.5}
                     20151127 {:date 20151127,:weight 98.2,:bf 13.0,:water 63.7,:muscle 43.4}
                     20151128 {:date 20151128,:weight 97.4,:bf 12.9,:water 63.9,:muscle 43.3}
                     20151129 {:date 20151129,:weight 97.5,:bf 13.1,:water 63.7,:muscle 43.3}
                     20151130 {:date 20151130,:weight 96.9,:bf 12.9,:water 63.9,:muscle 43.4}
                     20151201 {:date 20151201,:weight 97.3,:bf 12.9,:water 63.9,:muscle 43.3}
                     20151202 {:date 20151202,:weight 98.1,:bf 13.6,:water 63.3,:muscle 43.2}}))
