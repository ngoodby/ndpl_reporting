import arcgis
from arcgis.gis import GIS
import os, re, csv
portalURL = "https://www.arcgis.com"
username = "science@batcon.org"
password = "bats4ever"
survey_item_id = "01f064241e874a44aaa3167bff02f12c"
save_path = save_path = os.path.abspath(os.path.join(os.getcwd(),'..', 'data'))
keep_org_item = False
store_csv_w_attachments = False
gis = GIS(portalURL, username, password)
survey_by_id = gis.content.get(survey_item_id)
rel_fs = survey_by_id.related_items('Survey2Data','forward')[0]
item_excel = rel_fs.export(title=survey_by_id.title, export_format='Excel')
item_excel.download(save_path=save_path)
if not bool(keep_org_item):
    item_excel.delete(force=True)
