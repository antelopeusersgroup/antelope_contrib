# -*- coding: utf-8 -*-
#
# Copyright 2016 University of Nevada, Reno
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
"""
qmlutil.data

metadata shortcuts for the non-python data in the data folder
"""
import os

DATADIR = os.path.dirname(__file__)

QUAKEML_12_RELAXNG = os.path.join(DATADIR, "QuakeML-1.2.rng")
QUAKEML_BED_12_RELAXNG = os.path.join(DATADIR, "QuakeML-BED-1.2.rng")
QUAKEML_RT_12_RELAXNG = os.path.join(DATADIR, "QuakeML-RT-1.2.rng")
QUAKEML_BEDRT_12_RELAXNG = os.path.join(DATADIR, "QuakeML-RT-BED-1.2.rng")
QUAKEML_BED_12_XSD = os.path.join(DATADIR, "QuakeML-BED-1.2.xsd")
QUAKEML_BEDRT_12_XSD = os.path.join(DATADIR, "QuakeML-RT-BED-1.2.xsd")


