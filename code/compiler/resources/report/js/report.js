"use strict";

const INSPYER_URL = "https://insieme.github.io/inspyer/"

function cutoffWithElipsis(text, length) {
	var $span = $('<span>');

	if (text.length <= length) {
		return $span.text(text);
	}

	return $span.append(
		document.createTextNode(text.substr(0, length)),
		$('<a>')
			.addClass('ellipsis')
			.text('…')
			.click(function(e) {
				$(this).parent().text(text);
			})
	);
}

function demangle(text) {
	return text
		.replace(/IMP_/g, '')
		.replace(/_colon_/g, ':')
		.replace(/_comma_/g, ',')
		.replace(/_ampersand_/g, '&')
		.replace(/_operator_assign_/g, ' = ')
		.replace(/_operator_subscript_/g, '[]')
		.replace(/_operator_eq_/g, '==')
		.replace(/_operator_neq_/g, '!=')
		.replace(/_operator_lt_/g, '<')
		.replace(/_operator_le_/g, '<=')
		.replace(/_operator_gt_/g, '>')
		.replace(/_operator_ge_/g, '>=')
		.replace(/_lt_/g, '〈')
		.replace(/_gt_/g, '〉')
		.replace(/_space_/g, '⎵');
}

function determineLevelForEntry(entry) {
	var level = determineLevelFromIssues(entry.issues);

	for (var key in entry.variant_issues) {
		var level_ = determineLevelFromIssues(entry.variant_issues[key]);
		if (level_ == 'warning') {
			level = 'warning';
		} else if (level_ == 'danger') {
			level = 'danger';
			break;
		}
	}

	return level;
}

function determineLevelFromIssues(issues) {
	var level = 'success';

	for (var i = 0; i < issues.length; i++) {
		var issue = issues[i];
		if (determineLevelForIssue(issue) == 'warning') {
			level = 'warning';
		} else if (determineLevelForIssue(issue) == 'danger') {
			level = 'danger';
			break;
		}
	}

	return level;
}

function determineLevelForIssue(issue) {
	if (issue.severity == 'Error') {
		return 'danger';
	} else if (issue.severity == 'Warning') {
		return 'warning';
	} else {
		return 'info';
	}
}

function getHelpMessage(error_code) {
	if (!Array.isArray(report.help_messages)) {
		return "";
	}

	if (error_code in report.help_messages) {
		return report.help_messages[error_code];
	} else {
		return "";
	}
}

function setProgress() {
	var successes = 0;
	var warnings = 0;
	var errors = 0;
	var count = Object.keys(report.conversions).length;

	for (var addr in report.conversions) {
		var entry = report.conversions[addr];
		var level = determineLevelForEntry(entry);

		if (level == 'danger') {
			errors++;
		} else if (level == 'warning') {
			warnings++;
		} else {
			successes++;
		}
	}

	$('#progress-num').text(`${successes} / ${count}`);

	$('#progress .progress-bar-success').attr('style', `width: ${100 * successes / count}%`);
	$('#progress .progress-bar-warning').attr('style', `width: ${100 * warnings  / count}%`);
	$('#progress .progress-bar-danger').attr ('style', `width: ${100 * errors    / count}%`);
}

function inspyerLink(addr) {
	return $('<a>')
		.attr('href', `${INSPYER_URL}#node-${addr}`)
		.attr('target', 'inspyer')
		.addClass('inspyer-link internal')
		.tooltip({'placement': 'left', 'title': 'Open in INSPYER'})
		.html('<i class="glyphicon glyphicon-share-alt"></i>');
}

function createSource(id, addr, location, source) {

	var $src = $('<pre>').addClass('prism-collapse');
	var $head = $('<div>').addClass('location');
	var $link = $('<a>').attr('href', `#${id}`).attr('data-toggle', 'collapse');
	var $body = $('<code>').attr('id', id);

	if (location) {
		$src.attr('data-start', parseInt(location.substr(location.indexOf('@') + 1)));
		$head.append(
			$('<div>').addClass('nodeaddress internal').append(
				inspyerLink(addr),
				addr
			),
			$link.text(location)
		);
		$body.addClass('line-numbers');
	} else {
		$src.addClass('internal');
		$head.append(
			inspyerLink(addr),
			$link.addClass('nodeaddress').text(addr)
		);
	}

	if (source) {
		$body.addClass('language-cpp').text(source);
	} else {
		$body.addClass('language-txt').text('No location information');
	}

	return $src.append($head.append($link), $body);
}

function createBacktrace(addr, issue_index, issue, trace, index) {
	return createSource(
		`entry-${addr}-issue-${issue_index}-backtrace-${index}`,
		trace.address,
		trace.location,
		trace.source
	);
}

function createTags(issue) {
	if (!issue.tags) {
		return;
	}
	return $.map(issue.tags, function(tag) {
		return $('<div>').addClass('label label-default').text(tag);
	});
}

function createIssueDetails(addr, issue) {
	var $details = $('<div>').addClass('entry-issue-details');

	if (!issue.details) {
		return $details;
	}

	if (issue.details.type == 'message') {
		$details.append($('<pre>').text(issue.details.message));
	} else if (issue.details.type == 'data_requirements') {
		if (issue.details.unknown != 'false') {
			$details.text('Unknown Data Requirements');
		} else if (!issue.details.reqs) {
			$details.text('No Data Requirements discovered.');
		} else {

			$details.append(
				$.map(issue.details.reqs, function(req, index) {
					var id = `entry-${addr}-issue-req-${index}`;

					return $('<div>')
						.addClass('panel panel-default')
						.append(
							$('<div>').addClass('panel-heading').append(
								$('<a>').attr('href', `#${id}`).attr('data-toggle', 'collapse').text(`Data Requirement (${req.mode})`)
							),
							$('<div>').attr('id', id).addClass('panel-body collapse').append(
								$('<pre>').append($('<code>').addClass('language-cpp line-numbers').text(req.item)),
								$('<pre>').append($('<code>').addClass('language-cpp line-numbers').text(req.range))
							)
						)
				})
			)
		}

	} else {
		console.warn(`Unsupported details type: ${issue.details.type}`);
	}

	return $details;
}

function createIssue(addr, issue, index) {
	return $('<div>')
		.addClass(`entry-issue panel panel-${determineLevelForIssue(issue)}`)
		.addClass(
			$.map(issue.tags, function(tag) {
				return `tag-${tag}`;
			}).join(' ')
		)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<div>')
						.addClass('entry-issue-error-code')
						.text(issue.error_code),
					$('<div>')
						.addClass('entry-issue-message')
						.append(
							$('<a>')
								.attr('href', `#entry-${addr}-issue-${index}`)
								.attr('data-toggle', 'collapse')
								.addClass('panel-title')
								.append(
									$('<strong>').text(`${issue.severity}:`),
									' ',
									cutoffWithElipsis(demangle(issue.message), 100)
								)
						),
					$('<div>')
						.addClass('entry-issue-tags')
						.append(createTags(issue))
				),
			$('<div>')
				.attr('id', `entry-${addr}-issue-${index}`)
				.addClass('entry-issue-body collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							createIssueDetails(addr, issue),
							$('<div>')
								.addClass('entry-issue-help')
								.text(getHelpMessage(issue.error_code)),
							createSource(`entry-${addr}-issue-${index}-source`, issue.loc.address, issue.loc.location, issue.loc.source),
							$('<div>')
								.attr('id', `entry-${addr}-issue-${index}-backtrace`)
								.addClass('panel-group')
								.addClass('backtrace')
								.append(
									$('<div>').addClass('backtrace-text').text('Backtrace'),
									$.map(issue.backtrace, $.proxy(createBacktrace, null, addr, index, issue))
								)
						)
				)
		);
}

function createVariantIssues(addr, issues, variant) {
	var addr = `${addr}-variant-${variant}`;
	return $('<div>')
		.addClass(`entry-variant panel panel-${determineLevelFromIssues(issues)}`)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<a>')
						.attr('href', `#entry-${addr}`)
						.attr('data-toggle', 'collapse')
						.addClass('panel-title')
						.text(`Variant #${variant}`)
				),
			$('<div>')
				.attr('id', `entry-${addr}`)
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$.map(issues, $.proxy(createIssue, null, addr))
						)
				)
		);
}

function createConversion(entry, addr) {
	var loc = entry.loc_user ? entry.loc_user : entry.loc;

	return $('<div>')
		.addClass(`entry panel panel-${determineLevelForEntry(entry)}`)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<strong>').text(`#${entry.index}`),
					' ',
					$('<i>').addClass('glyphicon glyphicon-file'),
					' ',
					$('<a>')
						.attr('href', `#entry-${addr}`)
						.attr('data-toggle', 'collapse')
						.addClass('panel-title')
						.text(loc.location)
				),
			$('<div>')
				.attr('id', `entry-${addr}`)
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$.map(entry.issues, $.proxy(createIssue, null, addr)),
							$.map(entry.variant_issues, $.proxy(createVariantIssues, null, addr)),
							createSource(`entry-${addr}-source`, addr, loc.location, loc.source)
						)
				)
		);
}

function createRaw() {
	return $('<div>')
		.addClass('panel panel-default internal')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.attr('style', 'text-align: center')
				.append(
					$('<a>')
						.attr('href', '#raw')
						.attr('data-toggle', 'collapse')
						.text('Raw Conversion Report')
				),
			$('<div>')
				.attr('id', 'raw')
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>').text(JSON.stringify(report, null, 2))
						)
				)
		);
}

function setupControls() {
	// filter by tag
	{
		// collect all tags
		var tags = new Set();
		for (var addr in report.conversions) {
			var conversion = report.conversions[addr];
			for (var index = 0; index < conversion.issues.length; index++) {
				var issue = conversion.issues[index];
				if (issue.tags) {
					tags = new Set([...tags, ...issue.tags]);
				}
			}
			for (var key in conversion.variant_issues) {
				var issues = conversion.variant_issues[key];
				for (var index = 0; index < issues.length; index++) {
					var issue = issues[index];
					if (issue.tags) {
						tags = new Set([...tags, ...issue.tags]);
					}
				}
			}
		}
		tags = Array.from(tags);

		if (tags.length > 0) {
			$('#filters').append(
				$('<h4>').text('Filter Tags'),
				$.map(tags, function(tag) {
					return $('<div>')
						.addClass('checkbox')
						.append($('<label>').append(
							$('<input>')
								.attr('type', 'checkbox')
								.attr('checked', true)
								.change(function() {
									if (this.checked) {
										$(`.tag-${tag}`).show();
									} else {
										$(`.tag-${tag}`).hide();
									}
								}),
							tag
						))
				})
			);
		}

	}

	// export
	{
		var data = JSON.stringify(report, null, 2);
		$('a#export-button')
			.attr('download', 'report.json')
			.attr('href', `data:text/json;charset=utf-8,${escape(data)}`);
	}

	// internal
	{
		var $internals = $('.internal');

		$internals.hide();

		$('#internal-button').click(function() {
			$internals.toggle();
		});
	}

	// tooltips
	$('#controls [data-toggle="tooltip"]').tooltip();
}

function main() {
	if (Array.isArray(report.conversions)) {
		$('#main').append($.map(report.conversions, createConversion));

		// open if only 1
		if (Object.keys(report.conversions).length == 1) {
			for (var addr in report.conversions) {
				$(`#entry-${addr}`).collapse('show');
			}
		}
	} else {
		$('#no-entries').show();
		$('#progress').hide();
	}

	// add raw block
	$('#main').append(createRaw());

	setProgress();
	setupControls();

	// initialize Prism.js
	Prism.highlightAll();

	// fix initial collapse state of prism code blocks
	$('pre.prism-collapse code').addClass('collapse').attr('style', 'height: 0px');
}

$(document).ready(main);
